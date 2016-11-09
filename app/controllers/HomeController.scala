package controllers

import javax.inject._

import model.Tables
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameEntry._
import services.GogEntry.{getFromGog, getGogPageNumber}
import services.SteamEntry._
import services.{GogPageRetriever, SteamPageRetriever}

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class HomeController @Inject()(client: WSClient, configuration: Configuration, tables : Tables)(implicit exec: ExecutionContext) extends Controller {

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)

  def index = Action.async {
    Future {
      Ok(views.html.index(""))
    }
  }

  def gogData() = Action.async { implicit request =>
    gogRetriever.retrieve().map(getGogPageNumber).flatMap(gogRetriever.retrievePages).flatMap(getFromGog(tables)).map(d => Ok(Json.toJson(d)))
  }

  def steamData() = Action.async { implicit request =>
    steamRetriever.retrieve().map(getFromSteam).map(d => Ok(Json.toJson(d)))
  }


}
