package actors

import actors.MyWebSocketActor.RefreshUserData
import actors.ScheduleActor.{RefreshGames, RefreshPrices}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.google.inject.Inject
import model.{Tables, User}
import play.api.Configuration
import play.api.libs.ws.WSClient
import services._

import scala.concurrent.duration.{FiniteDuration, MINUTES, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

object ScheduleActor {

  case class RefreshGames()

  case class RefreshPrices()

}

class ScheduleActor @Inject()(system : ActorSystem, client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Actor {
  val steamActors: Array[ActorRef] = (1 to 2).map(_ => system.actorOf(Props(classOf[SteamRefreshActor], client, tables, exec))).toArray
  val gogActors: Array[ActorRef] = (1 to 2).map(_ => system.actorOf(Props(classOf[GogRefreshActor], client, tables, configuration, exec))).toArray
  val priceActors: Array[ActorRef] = (1 to 2).map(_ => system.actorOf(Props(classOf[PriceRefreshActor], client, tables, exec))).toArray

  override def receive: Receive = {
    case _: RefreshGames =>
      Await.result(refreshUsers(steamActors, SteamRefreshActor.RunRefresh.apply), FiniteDuration(5, MINUTES))
      Await.result(refreshUsers(gogActors, GogRefreshActor.RunRefresh.apply), FiniteDuration(5, MINUTES))
      system.actorSelection("akka://application/user/*") ! RefreshUserData()
      println("Refreshed games")
    case _: RefreshPrices =>
      Await.result(refreshPrices(), FiniteDuration(5, MINUTES))
      println("Refreshed prices")
  }

  private def refreshUsers(actors: Array[ActorRef], refresh: (User) => Any): Future[Unit] = {
    def scheduleRefresh(user : User, i : Int) : Unit =
      system.scheduler.scheduleOnce(FiniteDuration(1, SECONDS), actors(i % actors.length), refresh(user))
    tables.getAllUsers.map(users => users.zipWithIndex.foreach((scheduleRefresh _).tupled))
  }

  private def refreshPrices(): Future[Unit] = {
    def scheduleRefresh(e : Seq[SteamEntry], i : Int) : Unit =
      system.scheduler.scheduleOnce(FiniteDuration(1, SECONDS), priceActors(i % priceActors.length), PriceRefreshActor.RunRefresh(e))
    tables.getSteamEntries(None, None).map(entries => entries.grouped(20).toSeq.zipWithIndex.foreach((scheduleRefresh _).tupled))
  }
}