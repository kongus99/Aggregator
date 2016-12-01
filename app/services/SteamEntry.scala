package services

import model.Tables
import org.jsoup.Jsoup
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import services.GameEntry._
import services.GameSources.GameSources

import scala.concurrent.{ExecutionContext, Future}

case class SteamEntry(name : String, steamId : Long, price : Option[Float] = None)

object SteamEntry{
  private val regExp = "var rgGames = (.+);".r

  implicit val steamReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) ((n, i) => SteamEntry(n, i))
  implicit val steamWrites: Writes[SteamEntry] = (
      (JsPath \ "name").write[String] and
      (JsPath \ "steamId").write[Long] and
      (JsPath \ "price").write[Option[Float]])((e) => (e.name, e.steamId, e.price))

  def getFromSteam(tables : Tables)(owned: String, wishList : String, sources : GameSources)(implicit exec: ExecutionContext): Future[Seq[GameEntry]] = {
    val parsed = parseOwned(owned) ++ parseWishList(wishList)
    tables.replaceSteamData(parsed).flatMap(_ => generateFromNames(sources, tables))
  }

  private def parseWishList(wishList: String) = {
    import scala.collection.JavaConversions._
    val items = Jsoup.parse(wishList).getElementById("wishlist_items").getElementsByClass("wishlistRow").toList
    items.map(e => {
      val id = e.attr("id").split("_")(1)
      val name = e.getElementsByAttributeValue("class", "ellipsis").text()
      val price = e.getElementsByClass("price").text().split('€')(0).replace(",", ".").toFloat
      SteamEntry(name, id.toLong, Some(price))
    })
  }

  private def parseOwned(owned: String) = {
    Json.parse(regExp.findAllMatchIn(owned).map(m => m.group(1)).next()).validate[List[SteamEntry]].get
  }
}
