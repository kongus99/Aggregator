package controllers

import javax.inject._

import model.Tables
import play.api.Configuration
import play.api.libs.json.{JsArray, Json, Writes}
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GogEntry._
import services.SteamEntry._
import services._

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

  def limitToClose(gog: Seq[GogEntry], steam: Seq[SteamEntry]) = {
    val tuples = gog.flatMap(g => steam.map(s => (g, s))).map(p => (p._1, p._2, ThresholdLevenshtein.count(p._1.title, p._2.name, 2))).filter(t => t._3 < 2)
    (tuples.map(_._1), tuples.map(_._2))
  }

  def getData = {
    for {
      gog <- tables.getGogEntries
      steam <- tables.getSteamEntries
    } yield {
      limitToClose(gog.sortBy(_.title), steam.sortBy(_.name))
    }
  }
}