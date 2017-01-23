package services

import java.net.URL

import model.{Tables, User}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}

case class PriceEntry(steamId: Long, name : String, host : String, link: String, price: BigDecimal)

object PriceEntry {
  def addArgumentToFuture[A, B](t: (A, Future[B]))(implicit exec: ExecutionContext): Future[(A, B)] = t._2.map(r => (t._1, r))

  import play.api.libs.functional.syntax._

  implicit val steamWrites: Writes[PriceEntry] = (
    (JsPath \ "steamId").write[Long] and
      (JsPath \ "name").write[String] and
      (JsPath \ "host").write[String] and
      (JsPath \ "link").write[String] and
      (JsPath \ "price").write[BigDecimal]) ((e) => (e.steamId, e.name, e.host, e.link, e.price))

  def getPrices(tables: Tables,
                user : Option[User],
                golRetriever: String => Future[String],
                fkRetriever : String => Future[String],
                keyeRetriever : String => Future[String])(implicit exec: ExecutionContext): Future[Map[Long, Seq[PriceEntry]]] = {
    for {
      entries <- tables.getSteamEntries(user, Some(false))
      golPrices <- GolPricesFetcher.getPrices(entries, tables, golRetriever)
      fkPrices <- FKPricesFetcher.getPrices(entries, tables, fkRetriever)
      keyePrices <- KeyePricesFetcher.getPrices(entries, tables, keyeRetriever)
    } yield {
      (fkPrices ++ keyePrices ++ golPrices).groupBy(_.steamId).map(e => (e._1, e._2.sortBy(_.price).take(5)))
    }
  }
}

object KeyePricesFetcher {
  import play.api.libs.functional.syntax._

  private val keyePriceReads = ((JsPath \ "name").read[String] and (JsPath \ "price").read[String] and (JsPath \ "url").read[String])((n, p, u) => (n, p, u))

  private val host = "https://www.keye.pl"
  def getPrices(entries: Seq[SteamEntry], tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {
    def autoCompleteUrl(query: String) = s"/index/lists?term=$query"

    for {
      complete <- Future.sequence(entries.map(e => retriever(autoCompleteUrl(e.name)).map(s => (e, s))))
    } yield {
      def parsePrice(steamEntry: SteamEntry, json: String): PriceEntry = {
        val parse = Json.parse(json)
        val data = parse.as[List[JsValue]].map(_.as(keyePriceReads)).head
        PriceEntry(steamEntry.steamId, data._1, host, host + data._3, BigDecimal(data._2).setScale(2))
      }

      complete.filter(p => !p._2.isEmpty && p._2 != "[]").map((parsePrice _).tupled)
    }
  }
}

object FKPricesFetcher {

  import scala.collection.JavaConversions._
  val host = "www.fabrykakluczy.pl"

  def getPrices(entries: Seq[SteamEntry], tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {
    def getLinks = {
      def autoCompleteUrl(query : String) = s"/search/?q=$query"
      for {
        complete <- Future.sequence(entries.map(e => retriever(autoCompleteUrl(e.name)).map(s => (e, s))))
      } yield {
        def parseUrl(name: String, html: String): Seq[String] = {
          val candidates = Jsoup.parse(html).getElementsByTag("a").toList
          val winner = candidates.map(a => (ThresholdLevenshtein.count(a.text(), name, 100), a)).sortBy(_._1).head._2
          Seq(winner.attr("href").split(host)(1))
        }
        complete.filter(p => !p._2.isEmpty).flatMap(p => parseUrl(p._1.name, p._2).map(s => (p._1, s)))
      }
    }

    for {
      links <- getLinks
      details <- Future.sequence(links.map(e => retriever(e._2).map(s => (e._1, s))))
    } yield{
      def getPrice(e: SteamEntry, page : String) = {
        val parsed = Jsoup.parse(page)
        val price = parsed.getElementById("gameinfo").getElementsByClass("price").head.text().split(" ")(0)
        val activeContent = parsed.getElementById("content").getElementsByClass("active").head
        val name = activeContent.text()
        val link = activeContent.getElementsByTag("a").head.attr("href")
        PriceEntry(e.steamId, name, host, link, BigDecimal(price))
      }
      details.map((getPrice _).tupled)
    }
  }

}

object GolPricesFetcher{

  import services.PriceEntry.addArgumentToFuture

  import scala.collection.JavaConversions._

  def getPrices(entries : Seq[SteamEntry], tables: Tables, retriever: String => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {

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
        details <- getGolIds(entries).flatMap(queries => Future.sequence(queries.map(q => addArgumentToFuture(q))))
      } yield {
        details.filter((onlyContainingPrices _).tupled).map({ case (e, p) => (e, retriever(allPricesSearchUrlPrefix(parsePricesId(p)))) })
      }
    }

    for {
      prices <- getPriceMiniatures(entries).flatMap(queries => Future.sequence(queries.map(q => addArgumentToFuture(q))))
    } yield {
      def getPrice(e: SteamEntry)(priceElement : Element) = {
        val price = BigDecimal(priceElement.getElementsByClass("gpcl-cen").text().split(" ")(0).replaceAll(",", ".")).setScale(2)
        val name = priceElement.getElementsByClass("gpcl-tit").head.getElementsByTag("p").head.text()
        val host =  new URL(priceElement.attr("onclick").split("'")(1)).getHost
        val link =  priceElement.attr("onclick").split("'")(1)
        PriceEntry(e.steamId, name, host, link, price)
      }
      prices.flatMap({case (s, p) => Jsoup.parse(p).getElementsByClass("gpc-lista-a").toList.map(getPrice(s))})
    }
  }
}
