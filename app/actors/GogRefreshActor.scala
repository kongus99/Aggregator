package actors

import actors.GogRefreshActor.RunRefresh
import actors.ScheduleActor.UserGamesRefreshed
import akka.actor.Actor
import model.{CurrencyConverter, Tables, User}
import play.api.Configuration
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
      refreshGogGames(user).onSuccess({case _ => s ! UserGamesRefreshed(user)})
  }

  private def refreshGogGames(user: User): Future[Any] = {
    for {
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future(""))
      refreshed <- tables.replaceGogData(user, GogEntry.parse(owned, wishlist, rates))
    } yield {
      refreshed
    }
  }
}
