package services

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}

case class GameOptions(entry: SteamEntry, queries: Seq[GameQuery])

case class GameQuery(query: String, site: String, private val allResults: Seq[String], selectedResult: Option[String]){
  val results: Seq[String] = allResults.take(10)
}

object GameOptions {
  implicit val gameOptionsWrites: Writes[GameOptions] =
    ((JsPath \ "entry").write[SteamEntry] and
      (JsPath \ "queries").write[Seq[GameQuery]]) ((o) => (o.entry, o.queries))
}

object GameQuery {
  implicit val gameQueryWrites: Writes[GameQuery] =
    ((JsPath \ "query").write[String] and
      (JsPath \ "site").write[String] and
      (JsPath \ "results").write[Seq[String]] and
      (JsPath \ "selectedResult").write[Option[String]]) ((q) => (q.query, q.site, q.results, q.selectedResult))
}