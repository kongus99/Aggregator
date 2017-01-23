package modules

import akka.actor.{Actor, ActorSystem}
import com.google.inject.Inject
import model.{CurrencyConverter, Tables}
import modules.ScheduleActor.{RefreshGog, RefreshPrices, RefreshSteam}
import play.api.Configuration
import play.api.libs.ws.WSClient
import services.GogEntry.getGogPageNumber
import services._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object ScheduleActor {

  case class RefreshSteam()
  case class RefreshGog()
  case class RefreshPrices()

}

class ScheduleActor @Inject()(actorSystem: ActorSystem, client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Actor {
  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val ratesRetriever = new ReferenceRatesRetriever(client)
  val golRetriever = new GolRetriever(client)
  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)

  override def receive: Receive = {
    case _ : RefreshSteam =>
      Await.result(refreshSteamGames(1L), Duration.Inf)
      println("Refreshed steam")
    case _ : RefreshGog =>
      Await.result(refreshGogGames(1L), Duration.Inf)
      println("Refreshed gog")
    case _ : RefreshPrices =>
      Await.result(refreshPrices(1L), Duration.Inf)
      println("Refreshed prices")
  }

  private def refreshGogGames(userId: Long): Future[Any] = {
    for {
      user <- tables.getUserById(userId)
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.map(u => u.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future(""))).getOrElse(Future(""))
      refreshed <- tables.replaceGogData(user, GogEntry.parse(owned, wishlist, rates))
    } yield {
      refreshed
    }
  }

  private def refreshSteamGames(userId: Long): Future[Any] = {
    for {
      user <- tables.getUserById(userId)
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      owned <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future(""))).getOrElse(Future(""))
      wishlist <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/wishlist")).getOrElse(Future(""))).getOrElse(Future(""))
      refreshed <- tables.replaceSteamData(user, SteamEntry.parse(owned, wishlist, rates))

    } yield {
      refreshed
    }
  }

  private def refreshPrices(userId: Long) : Future[Any] ={
    for {
      user <- tables.getUserById(userId)
      prices <- PriceEntry.getPrices(tables, user, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
      refreshed <- tables.replacePrices(prices.values.flatten.toSeq)
    } yield {
      refreshed
    }
  }

}