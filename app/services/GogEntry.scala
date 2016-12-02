package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json._
import services.GameEntry._
import services.GameSources.GameSources

import scala.concurrent.{ExecutionContext, Future}

case class GogEntry(title: String, gogId: Long, price: Option[Float] = None, discounted: Option[Float] = None)

object GogEntry {
  private val regExp = "var gogData = (.+);".r

  private val coreGogRead = (JsPath \ "title").read[String] and (JsPath \ "id").read[Long]
  val gogWishListReads: Reads[GogEntry] = (coreGogRead
    and (JsPath \ "price" \ "baseAmount").read[String]
    and (JsPath \ "price" \ "finalAmount").read[String])((t, i, p, d) => GogEntry(t, i, Some(p.toFloat), Some(d.toFloat)))
  val gogReads: Reads[GogEntry] = coreGogRead((t, i) => GogEntry(t, i))

  implicit val gogWrites: Writes[GogEntry] = (
    (JsPath \ "title").write[String] and
      (JsPath \ "gogId").write[Long] and
      (JsPath \ "price").write[Option[Float]] and
      (JsPath \ "discounted").write[Option[Float]])((e) => (e.title, e.gogId, e.price, e.discounted))

  private def parseWishList(wishList: String) = {
    parseGogEntries(gogWishListReads)(regExp.findAllMatchIn(wishList).map(m => m.group(1)).next())
  }

  def getFromGog(tables : Tables)(owned: Seq[String], wishList : String, sources : GameSources)(implicit exec: ExecutionContext): Future[Seq[GameEntry]] = {
    val parsed = owned.flatMap(parseGogEntries(gogReads)) ++ parseWishList(wishList)
    tables.replaceGogData(parsed).flatMap(_ => generateFromNames(sources, tables))
  }

  def getGogPageNumber(body: String): Int = (Json.parse(body) \ "totalPages").as[Int]

  private def parseGogEntries(reads: Reads[GogEntry])(body: String) = {
    val parse = Json.parse(body)
    (parse \ "products").as[List[JsValue]].map(_.as(reads))
  }
}
