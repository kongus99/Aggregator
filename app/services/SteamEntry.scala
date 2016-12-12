package services

import model.{Rates, Tables}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import services.GameEntry._
import services.GameSources.GameSources

import scala.concurrent.{ExecutionContext, Future}

case class SteamEntry(name: String, steamId: Long, price: Option[BigDecimal] = None, discounted: Option[BigDecimal]= None){
  def this(id: Long, n: String, p: Option[Float], d: Option[Float])= {
    this(n, id, p.map(v => BigDecimal(v)), d.map(v => BigDecimal(v)))
  }
}

object SteamEntry {
  private val regExp = "var rgGames = (.+);".r

  implicit val steamReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) ((n, i) => SteamEntry(n, i))
  implicit val steamWrites: Writes[SteamEntry] = (
    (JsPath \ "name").write[String] and
      (JsPath \ "steamId").write[Long] and
      (JsPath \ "price").write[Option[BigDecimal]]and
      (JsPath \ "discounted").write[Option[BigDecimal]]) ((e) => (e.name, e.steamId, e.price, e.discounted))

  def getFromSteam(tables : Tables)(owned: String, wishList : String, sources : GameSources, rates : Rates)(implicit exec: ExecutionContext): Future[Seq[GameEntry]] = {
    val parsed = parseOwned(owned) ++ parseWishList(wishList, rates)
    tables.replaceSteamData(parsed).flatMap(_ => generateFromNames(sources, tables))
  }

  private def parseWishList(wishList: String, rates: Rates) = {
    import scala.collection.JavaConversions._
    val items = Jsoup.parse(wishList).getElementById("wishlist_items").getElementsByClass("wishlistRow").toList
    items.map(e => {
      val id = e.attr("id").split("_")(1)
      val name = e.getElementsByAttributeValue("class", "ellipsis").text()
      val (price, discounted) = getPrices(e)
      SteamEntry(name, id.toLong, price.map(convert(rates)), discounted.map(convert(rates)))
    })
  }

  private def convert(rates: Rates)(value: String): BigDecimal = {
    rates.recalculateFromEuro(BigDecimal(value.split('€')(0).replace(",", ".")), "PLN").getOrElse(BigDecimal(-1))
  }

  private def getPrices(e: Element): (Option[String], Option[String]) = {
    val normalPrice = e.getElementsByClass("price").text()
    if (!normalPrice.isEmpty)
      (Some(normalPrice), None)
    else
      (Some(e.getElementsByClass("discount_original_price").text()), Some(e.getElementsByClass("discount_final_price").text()))
  }

  private def parseOwned(owned: String) = {
    Json.parse(regExp.findAllMatchIn(owned).map(m => m.group(1)).next()).validate[List[SteamEntry]].get
  }
}
