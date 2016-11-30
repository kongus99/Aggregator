package services

import model.Tables
import org.jsoup.Jsoup
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import services.GameEntry._

import scala.concurrent.{ExecutionContext, Future}

case class SteamEntry(name : String, steamId : Long, onWishList : Boolean = false)

object SteamEntry{
  private val regExp = "var rgGames = (.+);".r

  implicit val steamReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) ((n, i) => SteamEntry(n, i))
  implicit val steamWrites: Writes[SteamEntry] = (
      (JsPath \ "name").write[String] and
      (JsPath \ "steamId").write[Long] and
      (JsPath \ "onWishList").write[Boolean])((e) => (e.name, e.steamId, e.onWishList))

  def getFromSteam(tables : Tables)(owned: String, wishList : String)(implicit exec: ExecutionContext): Future[Seq[GameEntry]] = {
    val parsed = parseOwned(owned) ++ parseWishList(wishList)
    tables.replaceSteamData(parsed).flatMap(_ => generateFromNames(tables))
  }

  private def parseWishList(wishList: String) = {
    import scala.collection.JavaConversions._
    val items = Jsoup.parse(wishList).getElementById("wishlist_items").getElementsByClass("wishlistRow").toList
    items.map(e => {
      val id = e.attr("id").split("_")(1)
      val name = e.getElementsByAttributeValue("class", "ellipsis").text()
      SteamEntry(name, id.toLong, onWishList = true)
    })
  }

  private def parseOwned(owned: String) = {
    Json.parse(regExp.findAllMatchIn(owned).map(m => m.group(1)).next()).validate[List[SteamEntry]].get.map(_.copy(onWishList = false))
  }
}
