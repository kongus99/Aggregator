package controllers

import javax.inject._

import model.Tables
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameEntry.{generateFromNames, _}
import services.GameSources.GameSources
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MainController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)

  def main: Action[AnyContent] = Action.async {
    Future {
      Ok(views.html.main("Aggregator - summary", "javascripts/mainPage", "MainPage"))
    }
  }

  def getUserGames(userId: Long, sources: GameSources): Action[AnyContent] = Action.async {
    for {
      user <- tables.getUserById(userId)
      result <- generateFromNames(user, sources, tables)
      prices <- tables.getPrices(result.map(_.steam).filter(_.nonEmpty).flatten).map(_.groupBy(_.steamId).mapValues(_.sortBy(_.price)))
    } yield {
      Ok(Json.toJson(result.map(e => if (e.steam.isEmpty) e else e.copy(prices = prices.getOrElse(e.steam.head.steamId, Seq())))))
    }
  }


}
