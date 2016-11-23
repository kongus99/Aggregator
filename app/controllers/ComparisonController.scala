package controllers

import javax.inject._

import model.Tables
import play.api.Configuration
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Writes}
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.GameOn.GameOn
import services._

import scala.concurrent.{ExecutionContext, Future}


case class NamedEntry(internalId: Long, externalId: Long, name: String)

case class ComparisonEntry(left: NamedEntry, metricResult: Int, right: NamedEntry, matches: Boolean)

case class MatchEntry(leftOn: GameOn, rightOn: GameOn, leftId: Long, rightId: Long)

@Singleton
class ComparisonController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  implicit val namedWriter: Writes[NamedEntry] = (
    (JsPath \ "internalId").write[Long] and
      (JsPath \ "externalId").write[Long] and
      (JsPath \ "name").write[String]) ((e) => (e.internalId, e.externalId, e.name))

  implicit val comparisonWriter: Writes[ComparisonEntry] = (
    (JsPath \ "left").write[NamedEntry] and
      (JsPath \ "metricResult").write[Int] and
      (JsPath \ "right").write[NamedEntry] and
      (JsPath \ "matches").write[Boolean]) ((t) => (t.left, t.metricResult, t.right, t.matches))

  val gogRetriever = new GogPageRetriever(client, configuration)
  val steamRetriever = new SteamPageRetriever(client)

  def main: Action[AnyContent] = Action.async {
    Future {
      Ok(views.html.main("Aggregator - comparison", "javascripts/comparison.js", "Comparison"))
    }
  }

  def data(left: GameOn, right: GameOn, minimumMetric: Int): Action[AnyContent] = Action.async {
    getData(left, right, minimumMetric).map(p => Ok(Json.toJson(p)))
  }

  def limitToClose(left: Seq[(GameOn, NamedEntry)], right: Seq[(GameOn, NamedEntry)], minimumMetric: Int, allMatches: Map[(GameOn, GameOn), Set[(Long, Long)]]): Seq[ComparisonEntry] = {
    val cartesian: Seq[((GameOn, NamedEntry), (GameOn, NamedEntry))] = left.flatMap(g => right.map(s => (g, s)))
    cartesian.map(c => {
      val key = (c._1._1, c._2._1)
      val matchIds = allMatches.getOrElse(key, Set())
      val threshold = ThresholdLevenshtein.count(c._1._2.name, c._2._2.name, minimumMetric)
      ComparisonEntry(c._1._2, threshold, c._2._2, matches = matchIds.contains((c._1._2.externalId, c._2._2.externalId)))
    }).filter(t => t.metricResult < minimumMetric)
  }

  private def getData(left: GameOn, right: GameOn, minimumMetric: Int) = {
    for {
      leftEntries <- getEntries(left)
      rightEntries <- getEntries(right)
      allMatches <- tables.getAllMatches
    } yield {
      limitToClose(leftEntries.sortBy(_._2.name), rightEntries.sortBy(_._2.name), minimumMetric, allMatches)
    }
  }

  def getEntries(on: GameOn): Future[Seq[(GameOn, NamedEntry)]] = {
    on match {
      case GameOn.Gog => tables.getGogEntries.map(_.map(e => (GameOn.Gog, NamedEntry(e.id.getOrElse(0), e.gogId, e.title))))
      case GameOn.Steam => tables.getSteamEntries.map(_.map(e => (GameOn.Steam, NamedEntry(e.id.getOrElse(0), e.steamId, e.name))))
    }
  }

  def toggleMatch(leftOn: GameOn, rightOn: GameOn, leftExternalId: Long, rightExternalId: Long): Action[AnyContent] = Action.async {
    tables.changeMatch(MatchEntry(leftOn, rightOn, leftExternalId, rightExternalId)).map(r => Ok(Json.toJson("Ok")))
  }
}