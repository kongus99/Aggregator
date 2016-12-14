package services

import play.api.Configuration
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

abstract class PageRetriever(client: WSClient)(implicit exec: ExecutionContext) {
  val address: UrlAddress
  lazy val cookies: List[(String, String)] = address.cookies.map(c => "Cookie" -> c).toList

  def retrieve(): Future[String] = {
    client.url(address.url).withHeaders(cookies: _*).get().map(_.body)
  }

  def retrieve(query: String): Future[String] = {
    client.url(address.url + query).withHeaders(cookies: _*).get().map(_.body)
  }

  def retrievePages(number: Int): Future[Seq[String]] = {
    val addresses = (1 to number).map(address.url + "&page=" + _)
    Future.sequence(addresses.map(client.url(_).withHeaders(cookies: _*).get().map(_.body)))
  }

}

class GogPageRetriever(client: WSClient, configuration: Configuration)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  val address = UrlAddress(
    "https://www.gog.com/account/getFilteredProducts?hiddenFlag=0&mediaType=1",
    Some(configuration.underlying.getString("gog.cookies")))
}

class GogWishListRetriever(client: WSClient, configuration: Configuration)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  val address = UrlAddress("https://www.gog.com/u/kongus99/wishlist", None)
}

class SteamPageRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  val address = UrlAddress("http://steamcommunity.com/id/kongus/games/?tab=all", None)
}

class SteamWishListRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  val address = UrlAddress("http://steamcommunity.com/id/kongus/wishlist/", None)
}

class ReferenceRatesRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  val address = UrlAddress("http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml", None)
}

class GolRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  val address = UrlAddress("http://www.gry-online.pl", None)
}

class FKRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  val address = UrlAddress("http://www.fabrykakluczy.pl", None)
  private val header = ("Host", "www.fabrykakluczy.pl") ::
    ("X-Requested-With", "XMLHttpRequest") ::
    ("Referer", "http://www.fabrykakluczy.pl/") ::
    Nil

  override def retrieve(query: String): Future[String] = {
    client.url(address.url + query).withHeaders(header: _*).get().map(_.body)
  }
}

class KeyeRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  val address = UrlAddress("https://www.keye.pl", None)
}
