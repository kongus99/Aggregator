package services

import model.CurrencyConverter
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

import scala.collection.immutable.Seq

case class SteamEntry(name: String, steamId: Long, price: Option[BigDecimal] = None, discounted: Option[BigDecimal]= None, genres : String = "", tags : String = "", owned : Boolean) extends ShopEntry{
  val link = s"http://store.steampowered.com/app/$steamId"

  override def id: Long = steamId

  override def title: String = name
}

object SteamEntry {
  private val regExp = "var rgGames = (.+);".r

  implicit val steamReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) ((n, i) => SteamEntry(n, i, owned = true))
  implicit val steamWrites: Writes[SteamEntry] = (
    (JsPath \ "name").write[String] and
      (JsPath \ "steamId").write[Long] and
      (JsPath \ "link").write[String] and
      (JsPath \ "price" \ "normal").write[Option[BigDecimal]]and
      (JsPath \ "price" \ "discounted").write[Option[BigDecimal]] and
      (JsPath \ "genres").write[String] and
      (JsPath \ "tags").write[String]) ((e) => (e.name, e.steamId, e.link, e.price, e.discounted, e.genres, e.tags))

  def parseGenresAndTags(page: String): (Option[String], Option[String]) = {
    import scala.collection.JavaConversions._
    val detailsBlock = Jsoup.parse(page).getElementsByClass("block responsive_apppage_details_left game_details underlined_links").first()
    val genres =
      if (detailsBlock != null) {
        val allDetails = detailsBlock.getElementsByClass("details_block").first().children().toList.map(e => e.text()).filter(_.length() > 0)
        val genreIndex = allDetails.indexOf("Genre:")
        val devIndex = allDetails.indexOf("Developer:")
        if (genreIndex != -1 && devIndex > genreIndex)
          Some(allDetails.slice(genreIndex + 1, devIndex).mkString(","))
        else None
      } else None
    val tagsBlock = Jsoup.parse(page).getElementsByClass("glance_tags_ctn popular_tags_ctn").first()
    val tags =
      if (tagsBlock != null) {
        Some(tagsBlock.getElementsByTag("a").toList.map(e => e.text()).mkString(","))
      } else None

    (genres, tags)
  }

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
