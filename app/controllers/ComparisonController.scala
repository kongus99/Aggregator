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

@Singleton
class ComparisonController @Inject()(client: WSClient, configuration: Configuration, tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  implicit val namedWriter: Writes[NamedEntry] = (
    (JsPath \ "internalId").write[Long] and
      (JsPath \ "externalId").write[Long] and
      (JsPath \ "name").write[String])((e) => (e.internalId, e.externalId, e.name))

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

  def data(left : GameOn, right: GameOn) = Action.async {
      getData(left, right).map(p => Ok(Json.toJson(p)))
  }

  def limitToClose(gog: Seq[NamedEntry], steam: Seq[NamedEntry]): (Seq[NamedEntry], Seq[NamedEntry]) = {
    val tuples = gog.flatMap(g => steam.map(s => (g, s))).map(p => (p._1, p._2, ThresholdLevenshtein.count(p._1.name, p._2.name, 2))).filter(t => t._3 < 2)
    (tuples.map(_._1), tuples.map(_._2))
  }

  def getData(left : GameOn, right: GameOn) = {
    for {
      leftEntries <- getEntries(left)
      rightEntries <- getEntries(right)
    } yield {
      limitToClose(leftEntries.sortBy(_.name), rightEntries.sortBy(_.name))
    }
  }
  def getEntries(on : GameOn) : Future[Seq[NamedEntry]] ={
    on match {
      case GameOn.Gog => tables.getGogEntries.map(_.map(e => NamedEntry(e.id.getOrElse(0), e.gogId, e.title)))
      case GameOn.Steam => tables.getSteamEntries.map(_.map(e => NamedEntry(e.id.getOrElse(0), e.steamId, e.name)))
    }
  }

}