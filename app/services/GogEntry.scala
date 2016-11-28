package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import services.GameEntry._

import scala.concurrent.{ExecutionContext, Future}

case class GogEntry(title: String, gogId : Long)

object GogEntry{

  implicit val gogReads: Reads[GogEntry] = ((JsPath \ "title").read[String] and (JsPath \ "id").read[Long])((t, i) => GogEntry(t, i))
  implicit val gogWrites: Writes[GogEntry] = (
      (JsPath \ "title").write[String] and
      (JsPath \ "gogId").write[Long])((e) => (e.title, e.gogId))

  def getFromGog(tables : Tables)(data: Seq[String])(implicit exec: ExecutionContext): Future[Seq[GameEntry]] = {
    tables.replaceGogData(data.flatMap(parseGogEntries).toList).flatMap(_ => generateFromNames(tables))
  }

  def getGogPageNumber(body : String) : Int = (Json.parse(body) \ "totalPages").as[Int]

  def parseGogEntries(body: String): List[GogEntry] = (Json.parse(body) \ "products").validate[List[GogEntry]].get
}
