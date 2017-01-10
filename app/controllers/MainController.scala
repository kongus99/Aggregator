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

  def allData(userId : Long, sources: GameSources) = Action.async {
    for {
      user <- tables.getUserById(userId)
      result <- generateFromNames(user, sources, tables)
      prices <- PriceEntry.getPrices(tables, user, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
    } yield {
      Ok(Json.toJson(result.map(e => if (e.steam.isEmpty) e else e.copy(prices = prices.getOrElse(e.steam.head, Seq())))))
    }
  }

  def getGogGames(user : Option[User]) = {
    for{
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- user.map(u => u.gogLogin.map(l => gogWishListRetriever.retrieveWithUser(l)("/wishlist")).getOrElse(Future{""})).getOrElse(Future{""})
    } yield{
      (owned, wishlist)
    }
  }

  def getSteamGames(user: Option[User]) = {
    for{
      owned <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(l)("/games/?tab=all")).getOrElse(Future{""})).getOrElse(Future{""})
      wishlist <- user.map(u => u.steamLogin.map(l => steamRetriever.retrieveWithUser(l)("/wishlist")).getOrElse(Future{""})).getOrElse(Future{""})
    } yield{
      (owned, wishlist)
    }
  }

  def refreshUserGames(userId : Long, sources: GameSources) = Action.async {
    for {
      user <- tables.getUserById(userId)
      rates <- ratesRetriever.retrieve("").map(CurrencyConverter.parseFromXml)
      (gogOwned, gogWishlist) <- getGogGames(user)
      (steamOwned, steamWishlist) <- getSteamGames(user)
      _ <- tables.replaceGogData(user, GogEntry.parse(gogOwned, gogWishlist, rates))
      _ <- tables.replaceSteamData(user, SteamEntry.parse(steamOwned, steamWishlist, rates))
      result <- generateFromNames(user, sources, tables)
    } yield {
      Ok(Json.toJson(result))
    }
  }

}
