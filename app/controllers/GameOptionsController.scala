package controllers

import javax.inject.{Inject, Singleton}

import model.Tables
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller}
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GameOptionsController @Inject()(tables: Tables)(implicit exec: ExecutionContext) extends Controller {


  case class GameQuery(query: String, site: String, results: List[String], selectedResult: String)
  case class GameOptions(entry: SteamEntry, queries: List[GameQuery])

  import play.api.libs.functional.syntax._
  import play.api.libs.json.{JsPath, Writes}

  implicit val gameQueryWrites: Writes[GameQuery] =
    ((JsPath \ "query").write[String] and
      (JsPath \ "site").write[String] and
      (JsPath \ "results").write[Seq[String]] and
      (JsPath \ "selectedResult").write[String]) ((q) => (q.query, q.site, q.results, q.selectedResult))

  implicit val gameOptionsWrites: Writes[GameOptions] =
    ((JsPath \ "entry").write[SteamEntry] and
      (JsPath \ "queries").write[Seq[GameQuery]]) ((o) => (o.entry, o.queries))

  def fetch(steamId: Long): Action[AnyContent] = Action.async {
    val st = SteamEntry("Some Game", steamId, None, None, owned = true)
    val go = GameOptions(st, GameQuery("some query", "some site", "result1" :: "result2" :: Nil, "result2") :: Nil)
    Future(Ok(Json.toJson(go)))
  }

  def changeSearch(userId: Long, selectedResult : String, site : String, steamId : Long) = Action.async {
    Future(Ok(Json.toJson("Done.")))
  }

  def fetchSearch(userId: Long, query : String, site : String, steamId : Long) = Action.async {
    val st = SteamEntry("Some Game", steamId, None, None, owned = true)
    val go = GameOptions(st, GameQuery(query, site, "new 1" :: "new 2" :: "new 2" :: Nil, "") :: Nil)
    Future(Ok(Json.toJson(go)))
  }
}