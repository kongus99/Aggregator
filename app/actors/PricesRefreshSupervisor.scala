package actors

import java.util.UUID

import actors.PricesRefreshSupervisor.{RefreshedBatch, RunRefresh}
import actors.ScheduleActor.UserPricesRefreshed
import akka.actor.{Actor, ActorRef, Props}
import model.Tables
import play.api.libs.ws.WSClient
import services.{PriceEntry, SteamEntry}

import scala.concurrent.ExecutionContext

case class PriceRefreshResult(userId : Long, isSingle : Boolean, prices : Seq[PriceEntry])

object PricesRefreshSupervisor {

  case class RunRefresh()

  case class RefreshedBatch(id: UUID, prices: Seq[PriceEntry])

}

class PricesRefreshSupervisor(wishLists: Map[Long, Seq[SteamEntry]], client: WSClient, tables: Tables, implicit val exec: ExecutionContext) extends Actor {

  var workers: Set[UUID] = Set()

  var prices: Set[PriceEntry] = Set()

  var send: Option[ActorRef] = None

  override def receive: Receive = {
    case _: RunRefresh =>
      if (send.isEmpty) {
        send = Some(sender())
        wishLists.values.flatten.toSet.grouped(40).foreach(schedulePricesRefresh)
      }
    case r: RefreshedBatch =>
      prices = prices ++ r.prices
      workers = workers - r.id
      if (workers.isEmpty) {
        send.foreach(_ ! UserPricesRefreshed(prepareRefreshesForClients()))
        send = None
      }
  }

  private def schedulePricesRefresh(e: Set[SteamEntry]): Unit = {
    val id = java.util.UUID.randomUUID
    workers = workers + id
    context.actorOf(Props(classOf[PriceRefreshActor], e, id, client, tables, exec)) ! PriceRefreshActor.RunRefresh()
  }

  private def prepareRefreshesForClients(): Seq[PriceRefreshResult] = {
    val defaultPrices: Map[Long, Set[PriceEntry]] = prices.filter(_.userId == 1L)groupBy(p => p.steamId)
    val customizedPrices: Map[(Long, Long), Set[PriceEntry]] = prices.filter(_.userId != 1L).groupBy(p => (p.userId, p.steamId))
    wishLists.keys.map(k => {
      val wishList = wishLists.getOrElse(k, Seq())
      val prices = wishList.flatMap(e => customizedPrices.getOrElse((k, e.steamId), defaultPrices.getOrElse(e.steamId, Set())).toSeq)
      PriceRefreshResult(k, wishList.size == 1, prices)
    }).toSeq
  }
}