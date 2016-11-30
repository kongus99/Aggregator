package controllers

import javax.inject._

import model.Tables
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameEntry._
import services.GogEntry.{getFromGog, getGogPageNumber}
import services.SteamEntry.getFromSteam
import services.{GogPageRetriever, SteamPageRetriever, SteamWishListRetriever}

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class HomeController @Inject()(client: WSClient, configuration: Configuration, tables : Tables)(implicit exec: ExecutionContext) extends Controller {

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)
  val steamWishListRetriever = new SteamWishListRetriever(client)

  def main = Action.async {
    Future {
      Ok(views.html.main("Aggregator - summary", "javascripts/mainPage", "MainPage"))
    }
  }
  def allData() = Action.async {
    generateFromNames(tables).map(d => Ok(Json.toJson(d)))
  }


  def gogData() = Action.async {
    gogRetriever.retrieve().map(getGogPageNumber).flatMap(gogRetriever.retrievePages).flatMap(getFromGog(tables)).map(d => Ok(Json.toJson(d)))
  }

  def steamData() = Action.async {
    for{
      owned <- steamRetriever.retrieve()
      wishlist <- steamWishListRetriever.retrieve()
      result <- getFromSteam(tables)(owned, wishlist)
    } yield {
      Ok(Json.toJson(result))
    }
  }
}
