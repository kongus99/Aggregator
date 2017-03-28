package actors

import actors.PriceRefreshActor.RunRefresh
import actors.ScheduleActor.UserGamesRefreshed
import akka.actor.Actor
import model.Tables
import play.api.libs.json.{JsValue, Json}
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
      refreshPrices().onSuccess({case r => s ! UserGamesRefreshed(r)})
  }
  private def refreshPrices(): Future[Option[JsValue]] = {
    for {
      prices <- PriceEntry.getPrices(tables, entries, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
      _ <- tables.replacePrices(entries.map(_.steamId).toSet, prices.values.flatten.toSeq)
    } yield {
      //TODO : do not flatten - user preferences are lost this way !!!
      val pricesSeq = prices.values.flatten
      if(pricesSeq.isEmpty) None else Some(Json.toJson(prices.values.flatten))
    }
  }

}
