package controllers

import javax.inject._

import model.{CurrencyConverter, Tables}
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameEntry.{generateFromNames, _}
import services.GameSources.GameSources
import services.GogEntry.{getFromGog, getGogPageNumber}
import services.SteamEntry.getFromSteam
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class HomeController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

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

  def allData(sources: GameSources) = Action.async {
    for {
      user <- tables.getUserBySteamLogin(Some("kongus"))
      result <- generateFromNames(user, sources, tables)
      prices <- PriceEntry.getPrices(tables, user, golRetriever.retrieve, fkRetriever.retrieve, keyeRetriever.retrieve)
    } yield {
      Ok(Json.toJson(result.map(e => if (e.steam.isEmpty) e else e.copy(prices = prices.getOrElse(e.steam.head, Seq())))))
    }
  }

  def gogData(gogUserId : String, sources: GameSources) = Action.async {
    for {
      user <- tables.getUserBySteamLogin(Some("kongus"))
      owned <- gogRetriever.retrieve(getGogPageNumber)
      wishlist <- gogWishListRetriever.retrieveWithUser(gogUserId)("/wishlist")
      rates <- ratesRetriever.retrieve("")
      result <- getFromGog(tables)(user, owned, wishlist, sources, CurrencyConverter.parseFromXml(rates))
    } yield {
      Ok(Json.toJson(result))
    }
  }

  def steamData(steamUserId : String, sources: GameSources) = Action.async {
    for {
      user <- tables.getUserBySteamLogin(Some("kongus"))
      owned <- steamRetriever.retrieveWithUser(steamUserId)("/games/?tab=all")
      wishlist <- steamRetriever.retrieveWithUser(steamUserId)("/wishlist")
      rates <- ratesRetriever.retrieve("")
      result <- getFromSteam(tables)(user, owned, wishlist, sources, CurrencyConverter.parseFromXml(rates))
    } yield {
      Ok(Json.toJson(result))
    }
  }
}
