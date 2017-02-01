package actors

import actors.GogRefreshActor.RunRefresh
import akka.actor.Actor
import model.{CurrencyConverter, Tables, User}
import play.api.Configuration
import play.api.libs.ws.WSClient
import services.GogEntry.getGogPageNumber
import services._

import scala.concurrent.duration.{FiniteDuration, MINUTES}
import scala.concurrent.{Await, ExecutionContext, Future}

object GogRefreshActor {
  case class RunRefresh(user : User)
}

class GogRefreshActor (client: WSClient, tables: Tables, configuration: Configuration, implicit val exec: ExecutionContext) extends Actor {
  val gogRetriever = new GogPageRetriever(client, configuration)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val ratesRetriever = new ReferenceRatesRetriever(client)

  override def receive: Receive = {
    case r : RunRefresh =>
      Await.result(refreshGogGames(r.user), FiniteDuration(5, MINUTES))
      println("refreshed gog user " + r.user)
  }

  private def refreshGogGames(user: User): Future[Any] = {
    for {
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future(""))
      refreshed <- tables.replaceGogData(Some(user), GogEntry.parse(owned, wishlist, rates))
    } yield {
      refreshed
    }
  }
}
