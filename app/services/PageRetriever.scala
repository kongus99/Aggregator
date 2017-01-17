package services

import play.api.Configuration
import play.api.libs.ws.WSClient

import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

abstract class PageRetriever(client: WSClient)(implicit exec: ExecutionContext) {
  protected def address(username: String)(query: String): UrlAddress

  protected def alternateAddress(username: String)(query: String) : UrlAddress = UrlAddress("")

  def retrieve(query: String): Future[String] = call(address("")(query))

  def retrieveWithUser(useAlternate : Boolean)(username: String)(query: String): Future[String] = {
    val add = if (useAlternate) alternateAddress(username)(query) else address(username)(query)
    call(add)
  }

  private def call(add : UrlAddress): Future[String] = client.url(add.url).withHeaders(add.headers: _*).get().map(_.body)
}

class GogPageRetriever(client: WSClient, configuration: Configuration)(implicit exec: ExecutionContext) {
  lazy val address = UrlAddress(
    "https://www.gog.com/account/getFilteredProducts?hiddenFlag=0&mediaType=1",
    ("Cookie", configuration.underlying.getString("gog.cookies")) :: Nil)

  def retrieve(pageNumberParser: String => Int): Future[Seq[String]] = {
    def retrievePages(number: Int): Future[Seq[String]] = {
      val addresses = (1 to number).map(address.url + "&page=" + _)
      Future.sequence(addresses.map(client.url(_).withHeaders(address.headers: _*).get().map(_.body)))
    }

    client.url(address.url).withHeaders(address.headers: _*).get().map(_.body).map(pageNumberParser).flatMap(retrievePages)
  }
}

class GogWishListRetriever(client: WSClient, configuration: Configuration)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  override protected def address(user: String)(query: String): UrlAddress = UrlAddress(s"https://www.gog.com/u/$user$query")
}

class SteamPageRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  override protected def alternateAddress(user: String)(query: String): UrlAddress = UrlAddress(s"http://steamcommunity.com/profiles/$user$query")
  override protected def address(user: String)(query: String): UrlAddress = UrlAddress(s"http://steamcommunity.com/id/$user$query")
}

class ReferenceRatesRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  override protected def address(user: String)(query: String): UrlAddress = UrlAddress("http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")
}

class GolRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  override protected def address(user: String)(query: String): UrlAddress = UrlAddress("http://www.gry-online.pl" + query)
}

class FKRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  private val header = ("Host", "www.fabrykakluczy.pl") ::
    ("X-Requested-With", "XMLHttpRequest") ::
    ("Referer", "http://www.fabrykakluczy.pl/") ::
    Nil

  override def address(user: String)(query: String): UrlAddress = UrlAddress("http://www.fabrykakluczy.pl" + query, header)
}

class KeyeRetriever(client: WSClient)(implicit exec: ExecutionContext) extends PageRetriever(client) {
  override protected def address(user: String)(query: String) = UrlAddress("https://www.keye.pl" + query)
}
