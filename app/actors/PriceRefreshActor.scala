package actors

import actors.PriceRefreshActor.RunRefresh
import akka.actor.Actor
import model.Tables
import play.api.libs.ws.WSClient
import services._

import scala.concurrent.duration.{FiniteDuration, MINUTES}
import scala.concurrent.{Await, ExecutionContext, Future}


object PriceRefreshActor {
  case class RunRefresh(entries : Seq[SteamEntry])
}

class PriceRefreshActor (client: WSClient, tables: Tables, implicit val exec: ExecutionContext) extends Actor {
  val golRetriever = new GolRetriever(client)
  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)

  override def receive: Receive = {
    case r : RunRefresh =>
      Await.result(refreshPrices(r.entries), FiniteDuration(5, MINUTES))
      println("refreshed " + r.entries)
  }
  private def refreshPrices(entries : Seq[SteamEntry]): Future[Any] = {
    for {
      prices <- PriceEntry.getPrices(tables, entries, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
      refreshed <- tables.replacePrices(entries.map(_.steamId).toSet, prices.values.flatten.toSeq)
    } yield {
      refreshed
    }
  }

}
