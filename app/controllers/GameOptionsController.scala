package controllers

import javax.inject.{Inject, Singleton}

import model.Tables
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller}
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GameOptionsController @Inject()(tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  def fetch(gameId: Long): Action[AnyContent] = Action.async {
    import play.api.libs.functional.syntax._
    import play.api.libs.json.{JsPath, Writes}

    case class GameQuery(query: String, site: String, results: List[String], selectedResult: Int)
    case class GameOptions(entry: SteamEntry, queries: List[GameQuery])

    implicit val gameQueryWrites: Writes[GameQuery] =
      ((JsPath \ "query").write[String] and
        (JsPath \ "site").write[String] and
        (JsPath \ "results").write[Seq[String]] and
        (JsPath \ "selectedResult").write[Int]) ((q) => (q.query, q.site, q.results, q.selectedResult))

    implicit val gameOptionsWrites: Writes[GameOptions] =
      ((JsPath \ "entry").write[SteamEntry] and
        (JsPath \ "queries").write[Seq[GameQuery]]) ((o) => (o.entry, o.queries))

    val st = SteamEntry("Some Game", gameId, None, None, owned = true)
    val go = GameOptions(st, GameQuery("some query", "some site", "result1" :: "result2" :: Nil, 0) :: Nil)
    Future(Ok(Json.toJson(go)))
  }

  def changeSearch(userId: Long) = Action.async {
    Future(Ok(Json.toJson("Done.")))
  }
}