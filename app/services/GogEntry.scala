package services

import com.fasterxml.jackson.core.JsonParseException
import model.CurrencyConverter
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class GogEntry(title: String, link : String, gogId: Long, price: Option[BigDecimal] = None, discounted: Option[BigDecimal] = None, owned : Boolean) extends ShopEntry {
  override def id: Long = gogId
}

object GogEntry {
  private val regExp = "var gogData = (.+);".r

  private val coreGogRead = (JsPath \ "title").read[String] and (JsPath \ "url").read[String] and (JsPath \ "id").read[Long]
  def gogWishListReads(converter: CurrencyConverter): Reads[GogEntry] = (coreGogRead
    and (JsPath \ "price" \ "symbol").read[String]
    and (JsPath \ "price" \ "baseAmount").read[String]
    and (JsPath \ "price" \ "finalAmount").read[String])((t, u, i, s, p, d) => GogEntry(t, "http://www.gog.com" +  u, i, converter.convert(p + s), converter.convert(d + s), owned = false))
  val gogReads: Reads[GogEntry] = coreGogRead((t, u, i) => GogEntry(t, "http://www.gog.com" +  u, i, owned = true))

  implicit val gogWrites: Writes[GogEntry] = (
    (JsPath \ "title").write[String] and
      (JsPath \ "link").write[String] and
      (JsPath \ "gogId").write[Long] and
      (JsPath \ "price").write[Option[BigDecimal]] and
      (JsPath \ "discounted").write[Option[BigDecimal]])((e) => (e.title, e.link, e.gogId, e.price, if(e.price != e.discounted) e.discounted else None))

  private def parseWishList(wishList: String, converter : CurrencyConverter) = {
    val body = regExp.findAllMatchIn(wishList).map(m => m.group(1)).toSeq.headOption
    body.map(parseGogEntries(gogWishListReads(converter))).getOrElse(Seq())
  }

  def parse(owned: Seq[String], wishList : String, converter: CurrencyConverter): Seq[GogEntry] =
    owned.flatMap(parseGogEntries(gogReads)) ++ parseWishList(wishList, converter)

  def getGogPageNumber(body: String): Int = {
    try{
      (Json.parse(body) \ "totalPages").as[Int]
    } catch{
      case _ : JsonParseException => 0
    }
  }

  private def parseGogEntries(reads: Reads[GogEntry])(body: String) = {
    val parse = Json.parse(body)
    (parse \ "products").as[List[JsValue]].map(_.as(reads))
  }
}
