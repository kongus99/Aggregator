package actors

import actors.GogRefreshActor.RunRefresh
import actors.ScheduleActor.UserGamesRefreshed
import akka.actor.Actor
import model.{CurrencyConverter, Tables, User}
import play.api.Configuration
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.WSClient
import services.GogEntry.getGogPageNumber
import services._

import scala.concurrent.{ExecutionContext, Future}

object GogRefreshActor {
  case class RunRefresh()
}

class GogRefreshActor (user : User, client: WSClient, tables: Tables, configuration: Configuration, implicit val exec: ExecutionContext) extends Actor {
  val gogRetriever = new GogPageRetriever(client, configuration)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val ratesRetriever = new ReferenceRatesRetriever(client)

  override def receive: Receive = {
    case _ : RunRefresh =>
      val s = sender()
      refreshGogGames(user).onSuccess({case r => s ! UserGamesRefreshed(r)})
  }

  private def refreshGogGames(user: User): Future[Option[JsValue]] = {
    for {
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future(""))
      gogEntries = GogEntry.parse(owned, wishlist, rates)
      _ <- tables.replaceGogData(user, gogEntries)
    } yield {
      if(gogEntries.isEmpty) None else Some(Json.toJson(gogEntries))
    }
  }
}
