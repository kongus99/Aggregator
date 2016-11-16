package controllers

import javax.inject._

import model.Tables
import play.api.Configuration
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsArray, JsPath, Json, Writes}
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameOn.GameOn
import services._

import scala.concurrent.{ExecutionContext, Future}


case class NamedEntry(internalId : Long, externalId : Long, name : String)

case class ComparisonEntry(left : NamedEntry, metricResult : Int, right : NamedEntry)

@Singleton
class ComparisonController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  implicit val namedWriter: Writes[NamedEntry] = (
    (JsPath \ "internalId").write[Long] and
      (JsPath \ "externalId").write[Long] and
      (JsPath \ "name").write[String])((e) => (e.internalId, e.externalId, e.name))

  implicit val comparisonWriter: Writes[ComparisonEntry] = (
    (JsPath \ "left").write[NamedEntry] and
      (JsPath \ "metricResult").write[Int] and
      (JsPath \ "right").write[NamedEntry])((t) => (t.left, t.metricResult, t.right))

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)

  def main = Action.async {
    Future {
      Ok(views.html.main("Aggregator - comparison", "javascripts/comparison.js", "Comparison"))
    }
  }

  def data(left : GameOn, right: GameOn, minimumMetric : Int) = Action.async {
      getData(left, right, minimumMetric).map(p => Ok(Json.toJson(p)))
  }

  def limitToClose(gog: Seq[NamedEntry], steam: Seq[NamedEntry], minimumMetric : Int): Seq[ComparisonEntry] = {
    gog.flatMap(g => steam.map(s => (g, s))).map(p => ComparisonEntry(p._1, ThresholdLevenshtein.count(p._1.name, p._2.name, minimumMetric), p._2)).filter(t => t.metricResult < minimumMetric)
  }

  def getData(left : GameOn, right: GameOn, minimumMetric : Int) = {
    for {
      leftEntries <- getEntries(left)
      rightEntries <- getEntries(right)
    } yield {
      limitToClose(leftEntries.sortBy(_.name), rightEntries.sortBy(_.name), minimumMetric)
    }
  }
  def getEntries(on : GameOn) : Future[Seq[NamedEntry]] ={
    on match {
      case GameOn.Gog => tables.getGogEntries.map(_.map(e => NamedEntry(e.id.getOrElse(0), e.gogId, e.title)))
      case GameOn.Steam => tables.getSteamEntries.map(_.map(e => NamedEntry(e.id.getOrElse(0), e.steamId, e.name)))
    }
  }

}