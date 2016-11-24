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

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}


case class NamedEntry(externalId: Long, name: String)

case class ComparisonEntry(left: NamedEntry, metricResult: Int, right: NamedEntry, matches: Boolean)

case class MatchEntry(leftOn: GameOn, rightOn: GameOn, leftId: Long, rightId: Long)

@Singleton
class ComparisonController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  implicit val namedWriter: Writes[NamedEntry] = (
    (JsPath \ "id").write[Long] and
    (JsPath \ "name").write[String]) ((e) => (e.externalId, e.name))

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

  private def filterMirrored(toCheck: Seq[ComparisonEntry]) = {
    val mirrors = mutable.HashSet[(NamedEntry, NamedEntry)]()
    var result = mutable.Seq[ComparisonEntry]()
    toCheck.foreach(x => {
      val p = (x.left, x.right)
      if (!mirrors.contains(p)) {
        if(x.left.externalId != x.right.externalId)
          result = result :+ x
        mirrors += p += p.swap
      }
    })
    result.toList
  }

  def limitToClose(left: (GameOn, Seq[NamedEntry]), right: (GameOn, Seq[NamedEntry]), minimumMetric: Int, allMatches: Map[(GameOn, GameOn), Set[(Long, Long)]]): Seq[ComparisonEntry] = {
    val (leftOn, leftEntries) = left
    val (rightOn, rightEntries) = right
    val result = leftEntries.flatMap(g => rightEntries.map(s => (g, s))).map({ case (leftEntry, rightEntry) =>
      val threshold = ThresholdLevenshtein.count(leftEntry.name, rightEntry.name, minimumMetric)
      val matches = allMatches.getOrElse((leftOn, rightOn), Set()).contains((leftEntry.externalId, rightEntry.externalId))
      ComparisonEntry(leftEntry, threshold, rightEntry, matches)
    }).filter(t => t.metricResult < minimumMetric)
    if(leftOn == rightOn) filterMirrored(result) else result
  }

  private def getData(left: GameOn, right: GameOn, minimumMetric: Int) = {
    for {
      leftEntries <- getEntries(left)
      rightEntries <- getEntries(right)
      allMatches <- tables.getAllMatches
    } yield {
      limitToClose(leftEntries, rightEntries, minimumMetric, allMatches).sortBy(_.left.name)
    }
  }

  def getEntries(on: GameOn): Future[(GameOn, Seq[NamedEntry])] = {
    on match {
      case GameOn.Gog => tables.getGogEntries.map(s => (on, s.map(e => NamedEntry(e.gogId, e.title))))
      case GameOn.Steam => tables.getSteamEntries.map(s => (on, s.map(e => NamedEntry(e.steamId, e.name))))
    }
  }

  def toggleMatch(leftOn: GameOn, rightOn: GameOn, leftExternalId: Long, rightExternalId: Long): Action[AnyContent] = Action.async {
    tables.changeMatch(MatchEntry(leftOn, rightOn, leftExternalId, rightExternalId)).map(_ => Ok(Json.toJson("Ok")))
  }
}