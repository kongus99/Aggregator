package modules

import akka.actor.{Actor, ActorSystem}
import com.google.inject.Inject
import model.{CurrencyConverter, Tables, User}
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
    case _ : RefreshSteam => {
      val f = for {
        user <- tables.getUserById(1L)
        rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
        (steamOwned, steamWishlist) <- getSteamGames(user)
        refreshed <- tables.replaceSteamData(user, SteamEntry.parse(steamOwned, steamWishlist, rates))
      } yield {
        refreshed
      }
      Await.result(f, Duration.Inf)
      println("Refreshed steam")
    }
    case _ : RefreshGog => {
      val f = for {
        user <- tables.getUserById(1L)
        rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
        (gogOwned, gogWishlist) <- getGogGames(user)
        refreshed <- tables.replaceGogData(user, GogEntry.parse(gogOwned, gogWishlist, rates))
      } yield {
        refreshed
      }
      Await.result(f, Duration.Inf)
      println("Refreshed gog")
    }
    case _ : RefreshPrices => {
      val f = for {
        user <- tables.getUserById(1L)
        prices <- PriceEntry.getPrices(tables, user, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
        refreshed <- tables.replacePrices(prices.values.flatten.toSeq)
      } yield {
        refreshed
      }
      Await.result(f, Duration.Inf)
      println("Refreshed prices")
    }

  }

  private def getGogGames(user: Option[User]) = {
    for {
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.map(u => u.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future(""))).getOrElse(Future(""))
    } yield {
      (owned, wishlist)
    }
  }

  private def getSteamGames(user: Option[User]) = {
    for {
      owned <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future(""))).getOrElse(Future(""))
      wishlist <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/wishlist")).getOrElse(Future(""))).getOrElse(Future(""))
    } yield {
      (owned, wishlist)
    }
  }

}