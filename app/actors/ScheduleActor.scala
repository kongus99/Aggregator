package actors


import actors.MyWebSocketActor.RefreshUserData
import actors.ScheduleActor._
import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import com.google.inject.Inject
import model.{Tables, User}
import play.api.Configuration
import play.api.libs.json.{JsArray, Json, Writes}
import play.api.libs.ws.WSClient
import services._

import scala.concurrent.ExecutionContext

object ScheduleActor {

  case class RefreshGames()

  case class RefreshPrices()

  case class RefreshSteamGenresAndTags()

  case class RefreshPrice(userId : Long, steamId : Long)

  case class UserGamesRefreshed(userId : Long, data : (Seq[GameEntry], Seq[GameEntry]))

  case class UserPricesRefreshed(data : Seq[PriceRefreshResult])

}

class ScheduleActor @Inject()(system : ActorSystem, client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Actor {

  implicit def tuple2Writes[A, B](implicit a: Writes[A], b: Writes[B]): Writes[(A, B)] = new Writes[(A, B)] {
    def writes(tuple: (A, B)) = JsArray(Seq(a.writes(tuple._1), b.writes(tuple._2)))
  }

  def scheduleGamesRefresh(user : User) = {
    context.actorOf(Props(classOf[GameRefreshActor], user, client, tables, configuration, exec)) ! GameRefreshActor.RunRefresh()
  }

  def schedulePricesRefresh(e : Seq[(Long, SteamEntry)]) : Unit = {
    context.actorOf(Props(classOf[PricesRefreshSupervisor], e.groupBy(_._1).mapValues(_.map(_._2)), client, tables, exec)) ! PricesRefreshSupervisor.RunRefresh()
  }

  def scheduleGenresAndTagsRefresh(steamIds : Seq[Long]) : Unit ={
    steamIds.grouped(40).foreach(ids => context.actorOf(Props(classOf[SteamGameDetailsActor], ids, client, tables, exec)) ! SteamGameDetailsActor.RunRefresh())
  }

  override def receive: Receive = {
    case _: RefreshGames =>
      tables.getAllUsers.map(_.foreach(scheduleGamesRefresh))
    case _: RefreshPrices =>
      tables.getSteamWishLists().map(schedulePricesRefresh)
    case _: RefreshSteamGenresAndTags =>
      tables.getSteamEntries(None, None).map(_.map(_.steamId)).map(scheduleGenresAndTagsRefresh)
    case r : RefreshPrice =>
      tables.getSteamEntryById(r.steamId).map(e => schedulePricesRefresh(Seq((r.userId, e))))
    case r : UserGamesRefreshed =>
      system.actorSelection("akka://application/user/*") ! RefreshUserData(r.userId, Json.toJson(r.data))
      sender() ! PoisonPill
    case r : UserPricesRefreshed =>
      r.data.foreach(p => system.actorSelection("akka://application/user/*") ! RefreshUserData(p.userId, Json.toJson((p.wishListIds, p.prices))))
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