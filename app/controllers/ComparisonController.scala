package controllers

import javax.inject._

import model.Tables
import play.api.Configuration
import play.api.libs.json.{JsArray, Json, Writes}
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GogEntry._
import services.SteamEntry._
import services.{GogPageRetriever, SteamPageRetriever}

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class ComparisonController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)

  def main = Action.async {
    Future {
      Ok(views.html.main("Aggregator - comparison", "javascripts/comparison.js", "Comparison"))
    }
  }

  implicit def tuple2Writes[A, B](implicit a: Writes[A], b: Writes[B]): Writes[(A, B)] = new Writes[(A, B)] {
    def writes(tuple: (A, B)) = JsArray(Seq(a.writes(tuple._1), b.writes(tuple._2)))
  }

  def data() = Action.async {
      getData.map(p => Ok(Json.toJson(p)))
  }

  def getData = {
    for {
      gog <- tables.getGogEntries
      steam <- tables.getSteamEntries
    } yield {
      (gog, steam)
    }
  }
}