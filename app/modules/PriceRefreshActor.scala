package modules


import akka.actor.Actor
import model.Tables
import modules.PriceRefreshActor.RunRefresh
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
      refreshed <- tables.replacePrices(prices.values.flatten.toSeq)
    } yield {
      refreshed
    }
  }

}
