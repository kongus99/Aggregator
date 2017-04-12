package controllers

import javax.inject._

import model.Tables
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameEntry._
import services.GameSources.GameSources
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MainController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamCommunityPageRetriever(client)
  val gogWishListRetriever = new GogWishListRetriever(client, configuration)

  def main: Action[AnyContent] = Action.async {
    Future {
      Ok(views.html.main("Aggregator - summary", "javascripts/mainPage", "MainPage"))
    }
  }

  def getUserGames(userId: Long, sources: GameSources): Action[AnyContent] = Action.async {
    for {
      result <- gamesWithPrices(userId, sources, tables)
    } yield {
      Ok(Json.toJson(result))
    }
  }


}
