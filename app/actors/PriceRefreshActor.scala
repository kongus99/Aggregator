package actors

import actors.PriceRefreshActor.RunRefresh
import actors.ScheduleActor.UserGamesRefreshed
import akka.actor.Actor
import model.Tables
import play.api.libs.ws.WSClient
import services._

import scala.concurrent.{ExecutionContext, Future}


object PriceRefreshActor {
  case class RunRefresh()
}

class PriceRefreshActor (entries : Seq[SteamEntry], client: WSClient, tables: Tables, implicit val exec: ExecutionContext) extends Actor {
  val golRetriever = new GolRetriever(client)
  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)

  override def receive: Receive = {
    case _ : RunRefresh =>
      val s = sender()
      refreshPrices(entries).onSuccess({case _ => s ! UserGamesRefreshed(entries)})
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
