package services

import java.net.URL

import model.Tables
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.json.{JsPath, Writes}

import scala.concurrent.{ExecutionContext, Future}

case class PriceEntry(steamEntry: SteamEntry, host : String, link: String, price: BigDecimal)

object PriceEntry {

  def addArgumentToFuture[A, B](t: (A, Future[B]))(implicit exec: ExecutionContext): Future[(A, B)] = t._2.map(r => (t._1, r))

  import play.api.libs.functional.syntax._

  implicit val steamWrites: Writes[PriceEntry] = (
    (JsPath \ "steamId").write[Long] and
      (JsPath \ "host").write[String] and
      (JsPath \ "link").write[String] and
      (JsPath \ "price").write[BigDecimal]) ((e) => (e.steamEntry.steamId, e.host, e.link, e.price))

  def getPrices(tables: Tables)(retriever: String => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {
    for {
      entries <- tables.getSteamEntries(Some(true))
      golPrices <- GolPricesFetcher.getGolPrices(entries, tables, retriever)
    } yield {
      golPrices
    }
  }
}

object GolPricesFetcher{
  import services.PriceEntry.addArgumentToFuture

  import scala.collection.JavaConversions._

  def getGolPrices(entries : Seq[SteamEntry], tables: Tables, retriever: String => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {
    def getPrice(e: Element) = BigDecimal(e.getElementsByClass("gpcl-cen").text().split(" ")(0).replaceAll(",", ".")).setScale(2)
    def getLink(e: Element) = e.attr("onclick").split("'")(1)

    def getGolIds(entries : Seq[SteamEntry]): Future[Seq[(SteamEntry, Future[String])]] = {
      val nameSearchUrlPrefix = "/ajax/quicksearch.asp?qs="
      val miniPricesSearchUrlPrefix = "/ajax/porownywarka.asp?ID="
      def parseGameId(page: String) = Jsoup.parse(page).getElementsByTag("a").attr("href").split("=")(1)
      def onlyFullFinds(entry: SteamEntry, page: String) = {
        val parsed = Jsoup.parse(page)
        val fullText = parsed.getElementsByTag("a").text()
        val foundText = parsed.getElementsByTag("b").text()
        !fullText.isEmpty && fullText == foundText
      }
      for {
        autoComplete <- Future.sequence(entries.map(e => retriever(nameSearchUrlPrefix + e.name).map(s => (e, s))))
      } yield {
        autoComplete.filter((onlyFullFinds _).tupled).map({ case (e, p) => (e, retriever(miniPricesSearchUrlPrefix + parseGameId(p))) })
      }
    }

    def getPriceMiniatures(entries : Seq[SteamEntry]) = {
      def allPricesSearchUrlPrefix(gamePriceId: String) = s"/ajax/porownywarka_lista.asp?ID=$gamePriceId&ORDER=1&BOX=0&DIGITAL=1"
      def parsePricesId(page: String) = Jsoup.parse(page).getElementsByAttribute("href").attr("href").split("=")(1)
      def onlyContainingPrices(entry: SteamEntry, page: String) = Jsoup.parse(page).getElementById("PC_MAIN_CNT") != null
      for {
        details <- getGolIds(entries).flatMap(queries => Future.sequence(queries.map(addArgumentToFuture)))
      } yield {
        details.filter((onlyContainingPrices _).tupled).map({ case (e, p) => (e, retriever(allPricesSearchUrlPrefix(parsePricesId(p)))) })
      }
    }

    for {
      prices <- getPriceMiniatures(entries).flatMap(queries => Future.sequence(queries.map(addArgumentToFuture)))
    } yield {
      prices.flatMap({case (s, p) => Jsoup.parse(p).getElementsByClass("gpc-lista-a").toList.map(e => PriceEntry(s, new URL(getLink(e)).getHost, getLink(e), getPrice(e)))})
    }
  }




}
