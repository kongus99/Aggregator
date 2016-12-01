package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import services.GameEntry._
import services.GameSources.GameSources

import scala.concurrent.{ExecutionContext, Future}

case class GogEntry(title: String, gogId : Long, price : Option[Float] = None)

object GogEntry{
  private val regExp = "var gogData = (.+);".r

  implicit val gogReads: Reads[GogEntry] = ((JsPath \ "title").read[String] and (JsPath \ "id").read[Long])((t, i) => GogEntry(t, i))
  implicit val gogWrites: Writes[GogEntry] = (
      (JsPath \ "title").write[String] and
      (JsPath \ "gogId").write[Long] and
      (JsPath \ "price").write[Option[Float]])((e) => (e.title, e.gogId, e.price))

  private def parseWishList(wishList: String) = {
    parseGogEntries(regExp.findAllMatchIn(wishList).map(m => m.group(1)).next()).map(_.copy(price = Some(1.0f)))
  }

  def getFromGog(tables : Tables)(owned: Seq[String], wishList : String, sources : GameSources)(implicit exec: ExecutionContext): Future[Seq[GameEntry]] = {
    val parsed = owned.flatMap(parseGogEntries) ++ parseWishList(wishList)
    tables.replaceGogData(parsed).flatMap(_ => generateFromNames(sources, tables))
  }

  def getGogPageNumber(body : String) : Int = (Json.parse(body) \ "totalPages").as[Int]

  private def parseGogEntries(body: String) = (Json.parse(body) \ "products").validate[List[GogEntry]].get
}
