package actors


import actors.MyWebSocketActor.RefreshUserData
import actors.ScheduleActor.{RefreshGames, RefreshPrices, UserGamesRefreshed}
import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import com.google.inject.Inject
import model.{Tables, User}
import play.api.Configuration
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient
import services._

import scala.concurrent.ExecutionContext

object ScheduleActor {

  case class RefreshGames()

  case class RefreshPrices()

  case class UserGamesRefreshed(data : Option[JsValue])

}

class ScheduleActor @Inject()(system : ActorSystem, client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Actor {

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
      r.data.foreach(d => system.actorSelection("akka://application/user/*") ! RefreshUserData(d))
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