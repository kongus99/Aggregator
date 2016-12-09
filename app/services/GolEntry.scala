package services

import model.Tables
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.json.{JsPath, Writes}

import scala.concurrent.{ExecutionContext, Future}

case class GolEntry(steamEntry: SteamEntry, link: String, price: BigDecimal)

object GolEntry {

  import play.api.libs.functional.syntax._

  import scala.collection.JavaConversions._

  implicit val steamWrites: Writes[GolEntry] = (
    (JsPath \ "steamId").write[Long] and
      (JsPath \ "link").write[String] and
      (JsPath \ "price").write[BigDecimal]) ((e) => (e.steamEntry.steamId, e.link, e.price))

  val nameSearchUrlPrefix = "/ajax/quicksearch.asp?qs="
  val miniPricesSearchUrlPrefix = "/ajax/porownywarka.asp?ID="

  def allPricesSearchUrlPrefix(gamePriceId: String) = s"/ajax/porownywarka_lista.asp?ID=$gamePriceId&ORDER=1&BOX=0&DIGITAL=1"

  def getPrices(tables: Tables)(golRetriever: String => Future[String])(implicit exec: ExecutionContext): Future[Seq[GolEntry]] = {

    def addArgumentToFuture[A, B](t: (A, Future[B])): Future[(A, B)] = t._2.map(r => (t._1, r))

    def getGolIds = {
      for {
        entries <- tables.getSteamEntries(Some(true))
        games <- Future.sequence(entries.map(e => golRetriever(nameSearchUrlPrefix + e.name).map(s => (e, s))))
      } yield {
        games.filter((onlyFullFinds _).tupled).map({ case (e, p) => (e, golRetriever(miniPricesSearchUrlPrefix + parseGameId(p))) })
      }
    }

    def getPriceMiniatures = {
      for {
        details <- getGolIds.flatMap(queries => Future.sequence(queries.map(addArgumentToFuture)))
      } yield {
        details.filter((onlyContainingPrices _).tupled).map({ case (e, p) => (e, golRetriever(allPricesSearchUrlPrefix(parsePricesId(p)))) })
      }
    }

    for {
      prices <- getPriceMiniatures.flatMap(queries => Future.sequence(queries.map(addArgumentToFuture)))
    } yield {
      def getPrice(e: Element) = BigDecimal(e.getElementsByClass("gpcl-cen").text().split(" ")(0).replaceAll(",", ".")).setScale(2)
      def getLink(e: Element) = e.attr("onclick").split("'")(1)
      prices.flatMap({case (s, p) => Jsoup.parse(p).getElementsByClass("gpc-lista-a").toList.map(e => GolEntry(s, getLink(e), getPrice(e)))})
    }
  }

  private def parsePricesId(page: String) = Jsoup.parse(page).getElementsByAttribute("href").attr("href").split("=")(1)

  private def parseGameId(page: String) = Jsoup.parse(page).getElementsByTag("a").attr("href").split("=")(1)

  private def onlyContainingPrices(entry: SteamEntry, page: String) = Jsoup.parse(page).getElementById("PC_MAIN_CNT") != null

  private def onlyFullFinds(entry: SteamEntry, page: String) = {
    val parsed = Jsoup.parse(page)
    val fullText = parsed.getElementsByTag("a").text()
    val foundText = parsed.getElementsByTag("b").text()
    !fullText.isEmpty && fullText == foundText
  }
}
