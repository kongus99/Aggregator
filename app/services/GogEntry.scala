package services

import model.{CurrencyConverter, Tables}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import services.GameEntry._
import services.GameSources.GameSources

import scala.concurrent.{ExecutionContext, Future}

case class GogEntry(title: String, gogId: Long, price: Option[BigDecimal] = None, discounted: Option[BigDecimal] = None)

object GogEntry {
  private val regExp = "var gogData = (.+);".r

  private val coreGogRead = (JsPath \ "title").read[String] and (JsPath \ "id").read[Long]
  def gogWishListReads(converter: CurrencyConverter): Reads[GogEntry] = (coreGogRead
    and (JsPath \ "price" \ "symbol").read[String]
    and (JsPath \ "price" \ "baseAmount").read[String]
    and (JsPath \ "price" \ "finalAmount").read[String])((t, i, s, p, d) => GogEntry(t, i, Some(converter.convert(p + s)), Some(converter.convert(d + s))))
  val gogReads: Reads[GogEntry] = coreGogRead((t, i) => GogEntry(t, i))

  implicit val gogWrites: Writes[GogEntry] = (
    (JsPath \ "title").write[String] and
      (JsPath \ "gogId").write[Long] and
      (JsPath \ "price").write[Option[BigDecimal]] and
      (JsPath \ "discounted").write[Option[BigDecimal]])((e) => (e.title, e.gogId, e.price, e.discounted))

  private def parseWishList(wishList: String, converter : CurrencyConverter) = {
    parseGogEntries(gogWishListReads(converter))(regExp.findAllMatchIn(wishList).map(m => m.group(1)).next())
  }

  def getFromGog(tables : Tables)(owned: Seq[String], wishList : String, sources : GameSources, converter: CurrencyConverter)(implicit exec: ExecutionContext): Future[Seq[GameEntry]] = {
    val parsed = owned.flatMap(parseGogEntries(gogReads)) ++ parseWishList(wishList, converter)
    tables.replaceGogData(parsed).flatMap(_ => generateFromNames(sources, tables))
  }

  def getGogPageNumber(body: String): Int = (Json.parse(body) \ "totalPages").as[Int]

  private def parseGogEntries(reads: Reads[GogEntry])(body: String) = {
    val parse = Json.parse(body)
    (parse \ "products").as[List[JsValue]].map(_.as(reads))
  }
}
