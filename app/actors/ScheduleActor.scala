package actors


import actors.MyWebSocketActor.RefreshUserGames
import actors.ScheduleActor.{RefreshGames, RefreshPrices, UserGamesRefreshed, UserPricesRefreshed}
import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import com.google.inject.Inject
import model.{Tables, User}
import play.api.Configuration
import play.api.libs.json.{JsArray, Json, Writes}
import play.api.libs.ws.WSClient
import services.GameSources.GameSources
import services._

import scala.concurrent.ExecutionContext

object ScheduleActor {

  case class RefreshGames()

  case class RefreshPrices()

  case class UserGamesRefreshed(userId : Long, data : (GameSources, Seq[GameEntry]))

  case class UserPricesRefreshed(data : Seq[PriceEntry])

}

class ScheduleActor @Inject()(system : ActorSystem, client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Actor {

  implicit def tuple2Writes[A, B](implicit a: Writes[A], b: Writes[B]): Writes[(A, B)] = new Writes[(A, B)] {
    def writes(tuple: (A, B)) = JsArray(Seq(a.writes(tuple._1), b.writes(tuple._2)))
  }

  def scheduleGamesRefresh(user : User) = {
    context.actorOf(Props(classOf[GameRefreshActor], user, client, tables, configuration, exec)) ! GameRefreshActor.RunRefresh()
  }

  def schedulePricesRefresh(e : Seq[SteamEntry]) : Unit = {
    context.actorOf(Props(classOf[PriceRefreshActor], e, client, tables, exec)) ! PriceRefreshActor.RunRefresh()
  }

  override def receive: Receive = {
    case _: RefreshGames =>
      tables.getAllUsers.map(_.foreach(scheduleGamesRefresh))

    case _: RefreshPrices =>
      tables.getSteamEntries(None, None).map(entries => entries.grouped(40).foreach(schedulePricesRefresh))
    case r : UserGamesRefreshed =>
      system.actorSelection("akka://application/user/*") ! RefreshUserGames(r.userId, Json.toJson(r.data))
      sender() ! PoisonPill
    case r : UserPricesRefreshed =>
//      r.data.groupBy(_.userId).foreach({case (userId, prices) => system.actorSelection("akka://application/user/*") ! RefreshUserData(userId, Json.toJson(prices))})
      sender() ! PoisonPill

  }

//  override def supervisionStrategy = OneForOneStrategy() {
//    case _ => {
//      waitingFor -= sender()
//      if (waitingFor.isEmpty) ??? // processing finished
//      Stop
//    }
//  }

}