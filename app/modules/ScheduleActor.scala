package modules

import akka.actor.{Actor, ActorSystem}
import com.google.inject.Inject
import model.{CurrencyConverter, Tables, User}
import play.api.Configuration
import play.api.libs.ws.WSClient
import services.GameEntry.generateFromNames
import services.GogEntry.getGogPageNumber
import services._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object ScheduleActor {

  case object refreshSchedule

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
    case refreshSchedule => {
      val f = for {
        user <- tables.getUserById(1L)
        rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
        (gogOwned, gogWishlist) <- getGogGames(user)
        (steamOwned, steamWishlist) <- getSteamGames(user)
        result <- generateFromNames(user, GameSources.WishList, tables)
        _ <- tables.replaceGogData(user, GogEntry.parse(gogOwned, gogWishlist, rates))
        _ <- tables.replaceSteamData(user, SteamEntry.parse(steamOwned, steamWishlist, rates))
        prices <- PriceEntry.getPrices(tables, user, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
        _ <- tables.replacePrices(prices.values.flatten.toSeq)
      } yield {
        result.map(e => if (e.steam.isEmpty) e else e.copy(prices = prices.getOrElse(e.steam.head.steamId, Seq())))
      }
      Await.result(f, Duration.Inf)
      println("refreshed")
    }
  }

  private def getGogGames(user: Option[User]) = {
    for {
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.map(u => u.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future {
        ""
      })).getOrElse(Future {
        ""
      })
    } yield {
      (owned, wishlist)
    }
  }

  private def getSteamGames(user: Option[User]) = {
    for {
      owned <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future {
        ""
      })).getOrElse(Future {
        ""
      })
      wishlist <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/wishlist")).getOrElse(Future {
        ""
      })).getOrElse(Future {
        ""
      })
    } yield {
      (owned, wishlist)
    }
  }

}