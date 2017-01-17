package controllers

import javax.inject._

import model.{CurrencyConverter, Tables, User}
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameEntry.{generateFromNames, _}
import services.GameSources.GameSources
import services.GogEntry.getGogPageNumber
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MainController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val ratesRetriever = new ReferenceRatesRetriever(client)
  val golRetriever = new GolRetriever(client)
  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)

  def main = Action.async {
    Future {
      Ok(views.html.main("Aggregator - summary", "javascripts/mainPage", "MainPage"))
    }
  }

  private def getGogGames(user : Option[User]) = {
    for{
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.map(u => u.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(useAlternate = false)(l)("/wishlist")).getOrElse(Future{""})).getOrElse(Future{""})
    } yield{
      (owned, wishlist)
    }
  }

  private def getSteamGames(user: Option[User]) = {
    for{
      owned <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/games/?tab=all")).getOrElse(Future{""})).getOrElse(Future{""})
      wishlist <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(u.steamAlternate)(l)("/wishlist")).getOrElse(Future{""})).getOrElse(Future{""})
    } yield{
      (owned, wishlist)
    }
  }

  def getUserGames(userId : Long, sources: GameSources) = Action.async {
    for {
      user <- tables.getUserById(userId)
      result <- generateFromNames(user, sources, tables)
      prices <- tables.getPrices(result.map(_.steam).filter(_.nonEmpty).flatten).map(_.groupBy(_.steamId))
    } yield{
      Ok(Json.toJson(result.map(e => if (e.steam.isEmpty) e else e.copy(prices = prices.getOrElse(e.steam.head.steamId, Seq())))))
    }
  }

  def refreshUserGames(userId : Long, sources: GameSources) = Action.async {
    for {
      user <- tables.getUserById(userId)
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      (gogOwned, gogWishlist) <- getGogGames(user)
      (steamOwned, steamWishlist) <- getSteamGames(user)
      result <- generateFromNames(user, sources, tables)
      _ <- tables.replaceGogData(user, GogEntry.parse(gogOwned, gogWishlist, rates))
      _ <- tables.replaceSteamData(user, SteamEntry.parse(steamOwned, steamWishlist, rates))
      prices <- PriceEntry.getPrices(tables, user, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
      _ <- tables.replacePrices(prices.values.flatten.toSeq)
    } yield {
      Ok(Json.toJson(result.map(e => if (e.steam.isEmpty) e else e.copy(prices = prices.getOrElse(e.steam.head.steamId, Seq())))))
    }
  }

}
