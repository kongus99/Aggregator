package services

import model.CurrencyConverter
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

import scala.collection.immutable.Seq

case class SteamEntry(name: String, steamId: Long, price: Option[BigDecimal] = None, discounted: Option[BigDecimal]= None, owned : Boolean){
  val link = s"http://store.steampowered.com/app/$steamId"
}

object SteamEntry {
  private val regExp = "var rgGames = (.+);".r

  implicit val steamReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) ((n, i) => SteamEntry(n, i, owned = true))
  implicit val steamWrites: Writes[SteamEntry] = (
    (JsPath \ "name").write[String] and
      (JsPath \ "steamId").write[Long] and
      (JsPath \ "link").write[String] and
      (JsPath \ "price").write[Option[BigDecimal]]and
      (JsPath \ "discounted").write[Option[BigDecimal]]) ((e) => (e.name, e.steamId, e.link, e.price, e.discounted))

  def parse(owned: String, wishList : String, converter : CurrencyConverter): Seq[SteamEntry] =
    parseOwned(owned) ++ parseWishList(wishList, converter)

  private def parseWishList(wishList: String, converter: CurrencyConverter): Seq[SteamEntry] = {
    import scala.collection.JavaConversions._
    val wItems = Jsoup.parse(wishList).getElementById("wishlist_items")
    if (wItems != null) {
      val items = wItems.getElementsByClass("wishlistRow").toList
      items.map(e => {
        val id = e.attr("id").split("_")(1)
        val name = e.getElementsByAttributeValue("class", "ellipsis").text()
        val (price, discounted) = getPrices(e)
        SteamEntry(name, id.toLong, price.flatMap(converter.convert), discounted.flatMap(converter.convert), owned = false)
      })
    } else Seq()
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
