package controllers

import javax.inject._

import model.{Rates, Tables}
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
class HomeController @Inject()(client: WSClient, configuration: Configuration, tables : Tables)(implicit exec: ExecutionContext) extends Controller {

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)
  val steamWishListRetriever = new SteamWishListRetriever(client)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)
  val ratesRetriever = new ReferenceRatesRetriever(client)

  def main = Action.async {
    Future {
      Ok(views.html.main("Aggregator - summary", "javascripts/mainPage", "MainPage"))
    }
  }
  def allData(sources: GameSources) = Action.async {
    for{
      result <- generateFromNames(sources, tables)
    } yield {
      Ok(Json.toJson(result))
    }
  }

  def gogData(sources: GameSources) = Action.async {
    for{
      owned <- gogRetriever.retrieve().map(getGogPageNumber).flatMap(gogRetriever.retrievePages)
      wishlist <- gogWishListRetriever.retrieve()
      result <- getFromGog(tables)(owned, wishlist, sources)
    } yield {
      Ok(Json.toJson(result))
    }
  }

  def steamData(sources: GameSources) = Action.async {
    for{
      owned <- steamRetriever.retrieve()
      wishlist <- steamWishListRetriever.retrieve()
      rates <- ratesRetriever.retrieve()
      result <- getFromSteam(tables)(owned, wishlist, sources, Rates.parseFromXml(rates))
    } yield {
      Ok(Json.toJson(result))
    }
  }
}
