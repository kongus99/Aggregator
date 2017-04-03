package actors

import java.util.UUID

import actors.PriceRefreshActor.RunRefresh
import actors.PricesRefreshSupervisor.RefreshedBatch
import akka.actor.Actor
import model.Tables
import play.api.libs.ws.WSClient
import services._

import scala.concurrent.{ExecutionContext, Future}


object PriceRefreshActor {
  case class RunRefresh()
}

class PriceRefreshActor (entries : Set[SteamEntry], id : UUID, client : WSClient, tables: Tables, implicit val exec: ExecutionContext) extends Actor {
  val golRetriever = new GolRetriever(client)
  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)

  override def receive: Receive = {
    case _ : RunRefresh =>
      val s = sender()
      refreshPrices().onSuccess({case r => s ! RefreshedBatch(id, r)})
  }
  private def refreshPrices(): Future[Seq[PriceEntry]] = {
    for {
      prices <- PriceEntry.getPrices(tables, entries.toSeq, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
      _ <- tables.replacePrices(entries.map(_.steamId), prices.values.flatten.toSeq)
    } yield {
      prices.values.flatten.toSeq
    }
  }

}
