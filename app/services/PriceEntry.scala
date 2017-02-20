package services

import java.net.URL

import model.Tables
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.json._
import services.PriceHost.{FK, Keye}

import scala.concurrent.{ExecutionContext, Future}

case class PriceEntry(steamId: Long, name : String, host : String, link: String, price: BigDecimal)

case class PartialPrice(name : String, link : String, price : Option[String])


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
                entries : Seq[SteamEntry],
                golRetriever: String => Future[String],
                fkRetriever : String => Future[String],
                keyeRetriever : String => Future[String])(implicit exec: ExecutionContext): Future[Map[Long, Seq[PriceEntry]]] = {
    for {
      golPrices <- GolPricesFetcher.getPrices(entries, tables, golRetriever)
      fkPrices <- FKPricesFetcher.getPrices(entries, tables, fkRetriever)
      keyePrices <- KeyePricesFetcher.getPrices(entries, tables, keyeRetriever)
    } yield {
      (fkPrices ++ keyePrices ++ golPrices).groupBy(_.steamId).map(e => (e._1, e._2.sortBy(_.price).take(5)))
    }
  }
}

trait Fetcher {

}

object KeyePricesFetcher {
  import play.api.libs.functional.syntax._

  private val keyePriceReads = ((JsPath \ "name").read[String] and (JsPath \ "price").read[String] and (JsPath \ "url").read[String])((n, p, u) => PartialPrice(n, u, Some(p)))

  private def autoCompleteUrl(query: String) = s"/index/lists?term=$query"

  def getPrices(entries: Seq[SteamEntry], tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {
    for {
      complete <- Future.sequence(entries.map(e => getSuggestions(e.name, tables, retriever).map(s => (e, s))))
    } yield {
      def parsePrice(steamEntry: SteamEntry, suggestions: Seq[PartialPrice]): Option[PriceEntry] = {
        val data = suggestions.headOption
        data.map(d =>PriceEntry(steamEntry.steamId, d.name, "https://" + Keye.toString, "https://" + Keye.toString + d.link, BigDecimal(d.price.get).setScale(2)))
      }
      complete.filter(p => p._2.nonEmpty).map((parsePrice _).tupled).filter(_.isDefined).map(_.get)
    }
  }

  def getSuggestions(query: String, tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PartialPrice]] = {
    for {
      complete <- retriever(autoCompleteUrl(query))
    } yield {
      Json.parse(complete).as[List[JsValue]].map(_.as(keyePriceReads))
    }
  }
}

object FKPricesFetcher {

  import scala.collection.JavaConversions._

  private def autoCompleteUrl(query : String) = s"/search/?q=$query"

  def getPrices(entries: Seq[SteamEntry], tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {
    def getLinks = {
      for {
        complete <- Future.sequence(entries.map(e => getSuggestions(e.name, tables, retriever).map(s => (e, s))))
      } yield {
        def parseUrl(steamEntry: SteamEntry, suggestions: Seq[PartialPrice]): Option[PriceEntry] = {
          suggestions.map(s => (ThresholdLevenshtein.count(s.name, steamEntry.name, 100), s)).sortBy(_._1).headOption.map(_._2).map(p => PriceEntry(steamEntry.steamId, p.name, FK.toString, p.link, BigDecimal(0)))
        }
        complete.filter(p => p._2.nonEmpty).map((parseUrl _).tupled).filter(_.isDefined).map(_.get)
      }
    }

    for {
      links <- getLinks
      details <- Future.sequence(links.map(e => retriever(e.link.split(FK.toString)(1)).map(s => (e, s))))
    } yield{
      def getPrice(e: PriceEntry, page : String) = {
        val parsed = Jsoup.parse(page)
        val price = parsed.getElementById("gameinfo").getElementsByClass("price").head.text().split(" ")(0)
        val activeContent = parsed.getElementById("content").getElementsByClass("active").head
        val link = activeContent.getElementsByTag("a").head.attr("href")
        e.copy(link = link, price = BigDecimal(price))
      }
      details.map((getPrice _).tupled)
    }
  }

  def getSuggestions(query: String, tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PartialPrice]] = {
    for {
      complete <- retriever(autoCompleteUrl(query))
    } yield {
      Jsoup.parse(complete).getElementsByTag("a").toSeq.map(e => PartialPrice(e.text(), e.attr("href"), None))
    }
  }

}

object GolPricesFetcher{

  import services.PriceEntry.addArgumentToFuture

  import scala.collection.JavaConversions._

  private def autoCompleteUrl(query : String) = s"/ajax/quicksearch.asp?qs=$query"

  def getPrices(entries : Seq[SteamEntry], tables: Tables, retriever: String => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {

    def getGolIds(entries : Seq[SteamEntry]): Future[Seq[(SteamEntry, Future[String])]] = {
      val miniPricesSearchUrlPrefix = "/ajax/porownywarka.asp?ID="
      def parseGameId(page: String) = Jsoup.parse(page).getElementsByTag("a").attr("href").split("=")(1)
      def onlyFullFinds(entry: SteamEntry, page: String) = {
        val parsed = Jsoup.parse(page)
        val fullText = parsed.getElementsByTag("a").text()
        val foundText = parsed.getElementsByTag("b").text()
        !fullText.isEmpty && fullText == foundText
      }
      for {
        autoComplete <- Future.sequence(entries.map(e => retriever(autoCompleteUrl(e.name)).map(s => (e, s))))
      } yield {
        autoComplete.filter((onlyFullFinds _).tupled).map({ case (e, p) => (e, retriever(miniPricesSearchUrlPrefix + parseGameId(p))) })
      }
    }

    def getPriceMiniatures(entries : Seq[SteamEntry]) = {
      def allPricesSearchUrlPrefix(gamePriceId: String) = s"/ajax/porownywarka_lista.asp?ID=$gamePriceId&ORDER=1&BOX=0&DIGITAL=1"
      def parsePricesId(page: String) = Jsoup.parse(page).getElementsByAttribute("href").attr("href").split("=")(1)
      def onlyContainingPrices(entry: SteamEntry, page: String) = Jsoup.parse(page).getElementById("PC_MAIN_CNT") != null
      for {
        details <- getGolIds(entries).flatMap(queries => Future.sequence(queries.map(addArgumentToFuture(_))))
      } yield {
        details.filter((onlyContainingPrices _).tupled).map({ case (e, p) => (e, retriever(allPricesSearchUrlPrefix(parsePricesId(p)))) })
      }
    }

    for {
      prices <- getPriceMiniatures(entries).flatMap(queries => Future.sequence(queries.map(addArgumentToFuture(_))))
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

  def getSuggestions(query: String, tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[String]] = {
    for {
      complete <- retriever(autoCompleteUrl(query))
    } yield {
      Jsoup.parse(complete).getElementsByTag("a").toList.map(_.text())
    }
  }
}
