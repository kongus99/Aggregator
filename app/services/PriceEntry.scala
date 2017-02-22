package services

import java.net.URL

import model.Tables
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.json._
import services.PriceHost.{FK, Gol, Keye}

import scala.concurrent.{ExecutionContext, Future}

case class PriceEntry(steamId: Long, userId : Long, name: String, host: String, link: String, price: BigDecimal)

case class PartialPrice(name: String, link: String, price: Option[String])


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
                entries: Seq[SteamEntry],
                golRetriever: String => Future[String],
                fkRetriever: String => Future[String],
                keyeRetriever: String => Future[String])(implicit exec: ExecutionContext): Future[Map[Long, Seq[PriceEntry]]] = {
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
  val defaultSearchUserId = 1L

  private case class SearchHelper(steamEntry : SteamEntry, userId : Long, query : Option[String])

  private case class SuggestionHelper(searchHelper : SearchHelper, results : Seq[PartialPrice])

  def createPriceEntry(steamEntry: SteamEntry, userId : Long)(bestMatch : PartialPrice) : PriceEntry

  def getSuggestions(query: String, tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PartialPrice]]

  def site: String

  def getCompletes(entries: Seq[SteamEntry], tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] ={
    def resolveQuery(q : Option[String]) : Future[Seq[PartialPrice]] =
      q.map(getSuggestions(_, tables, retriever)).getOrElse(Future(Seq()))

    for {
      siteCues <- tables.getQueryData(site, entries.map(_.steamId).toSet)
      toSearch = allSearches(entries, siteCues)
      suggestions <- Future.sequence(toSearch.map(e => resolveQuery(e.query).map(SuggestionHelper(e, _))))
    } yield {
      suggestions.map(resolveSuggestionToPriceEntry).filter(_.isDefined).map(_.get)
    }
  }

  private def chooseBestPriceMatch(helper : SuggestionHelper) : Option[PartialPrice] = {
    if (helper.results.nonEmpty && helper.searchHelper.query.isDefined) {
      val exactMatch = helper.results.find(_.name == helper.searchHelper.query.get)
      if (exactMatch.isDefined) exactMatch else
      if (helper.results.size == 1) helper.results.headOption else
        helper.results.map(s => (ThresholdLevenshtein.count(s.name, helper.searchHelper.steamEntry.name, 100), s)).sortBy(_._1).headOption.map(_._2)
    } else None
  }

  private def resolveSuggestionToPriceEntry(suggestionHelper: SuggestionHelper) : Option[PriceEntry] = {
    if(suggestionHelper.results.nonEmpty)
      chooseBestPriceMatch(suggestionHelper).map(createPriceEntry(suggestionHelper.searchHelper.steamEntry, suggestionHelper.searchHelper.userId))
    else None
  }

  private def allSearches(entries: Seq[SteamEntry], siteCues: Map[Long, Seq[(Long, GameQuery)]]) : Seq[SearchHelper] = {
    val cuedSearches =
      for {
      e <- entries
      cues = siteCues.getOrElse(e.steamId, Seq())
      c <- cues
    } yield {
      SearchHelper(e, c._1, c._2.selectedResult)
    }
    val allSteamIds = entries.map(_.steamId).toSet
    val defaultSearches = cuedSearches.filter(_.userId == defaultSearchUserId).map(_.steamEntry.steamId).toSet
    val idsToAdd = allSteamIds.diff(defaultSearches)
    val additionalSearches =
      for {
      e <- entries.filter(e => idsToAdd.contains(e.steamId))
    } yield {
      SearchHelper(e, defaultSearchUserId, Some(e.name))
    }
    cuedSearches ++ additionalSearches
  }

}

object KeyePricesFetcher extends Fetcher{

  import play.api.libs.functional.syntax._

  private val keyePriceReads = ((JsPath \ "name").read[String] and (JsPath \ "price").read[String] and (JsPath \ "url").read[String]) ((n, p, u) => PartialPrice(n, u, Some(p)))

  private def autoCompleteUrl(query: String) = s"/index/lists?term=$query"

  def getPrices(entries: Seq[SteamEntry], tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {
    getCompletes(entries, tables, retriever)
  }

  override def getSuggestions(query: String, tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PartialPrice]] = {
    for {
      complete <- retriever(autoCompleteUrl(query))
    } yield {
      Json.parse(complete).as[List[JsValue]].map(_.as(keyePriceReads))
    }
  }

  override def createPriceEntry(steamEntry: SteamEntry, userId : Long)(bestMatch: PartialPrice): PriceEntry =
    PriceEntry(steamEntry.steamId, userId, bestMatch.name, "https://" + site, "https://" + site + bestMatch.link, BigDecimal(bestMatch.price.get).setScale(2))

  override val site: String = Keye.toString
}

object FKPricesFetcher extends Fetcher{

  import scala.collection.JavaConversions._

  private def autoCompleteUrl(query: String) = s"/search/?q=$query"

  def getPrices(entries: Seq[SteamEntry], tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {
    for {
      prices <- getCompletes(entries, tables, retriever)
      details <- Future.sequence(prices.map(e => retriever(e.link.split(site)(1)).map(s => (e, s))))
    } yield {
      def fixPrice(e: PriceEntry, page: String) = {
        val parsed = Jsoup.parse(page)
        val price = parsed.getElementById("gameinfo").getElementsByClass("price").head.text().split(" ")(0)
        val activeContent = parsed.getElementById("content").getElementsByClass("active").head
        val link = activeContent.getElementsByTag("a").head.attr("href")
        e.copy(link = link, price = BigDecimal(price))
      }

      details.map((fixPrice _).tupled)
    }
  }

  def getSuggestions(query: String, tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PartialPrice]] = {
    for {
      complete <- retriever(autoCompleteUrl(query))
    } yield {
      Jsoup.parse(complete).getElementsByTag("a").toSeq.map(e => PartialPrice(e.text(), e.attr("href"), None))
    }
  }

  override def createPriceEntry(steamEntry: SteamEntry, userId: Long)(bestMatch: PartialPrice): PriceEntry =
    PriceEntry(steamEntry.steamId, userId, bestMatch.name, site, bestMatch.link, BigDecimal(0))

  override def site: String = FK.toString
}

object GolPricesFetcher {

  import scala.collection.JavaConversions._

  private def autoCompleteUrl(query: String) = s"/ajax/quicksearch.asp?qs=$query"

  def getPrices(entries: Seq[SteamEntry], tables: Tables, retriever: String => Future[String])(implicit exec: ExecutionContext): Future[Seq[PriceEntry]] = {

    def getWinner(steamEntry: SteamEntry, suggestions: Seq[PartialPrice]): Option[PartialPrice] =
      suggestions.find(_.name == steamEntry.name)

    def parsePrice(steamEntry: SteamEntry, suggestions: Seq[PartialPrice]): Option[PriceEntry] =
      getWinner(steamEntry, suggestions).map(p => PriceEntry(steamEntry.steamId, 1L, p.name, Gol.toString, p.link, BigDecimal(0)))

    def changeLinkToPriceList(e: PriceEntry, page: String): Option[PriceEntry] = {
      def allPricesSearchUrlPrefix(gamePriceId: String) = s"/ajax/porownywarka_lista.asp?ID=$gamePriceId&ORDER=1&BOX=0&DIGITAL=1"

      val parsed = Jsoup.parse(page)
      val priceListForPc = parsed.getElementById("PC_PLT_1")
      val id = if(priceListForPc != null) priceListForPc.getElementsByClass("t5").headOption.map(_.attr("href").split("ID=")) else None
      id.filter(_.length > 1).map(s => e.copy(link = allPricesSearchUrlPrefix(s(1))))
    }

    def getPrice(e: PriceEntry)(priceElement: Element): PriceEntry = {
      val price = BigDecimal(priceElement.getElementsByClass("gpcl-cen").text().split(" ")(0).replaceAll(",", ".")).setScale(2)
      val name = priceElement.getElementsByClass("gpcl-tit").head.getElementsByTag("p").head.text()
      val host = new URL(priceElement.attr("onclick").split("'")(1)).getHost
      val link = priceElement.attr("onclick").split("'")(1)
      e.copy(link = link, name = name, host = host, price = price)
    }

    def miniPricesSearchUrlPrefix(id : String) = s"/ajax/porownywarka.asp?ID=$id"

    for {
      complete <- Future.sequence(entries.map(e => getSuggestions(e.name, tables, retriever).map(s => (e, s))))
      prices = complete.filter(p => p._2.nonEmpty).map((parsePrice _).tupled).filter(_.isDefined).map(_.get)
      pricesWithDetails <- Future.sequence(prices.map(e => retriever(miniPricesSearchUrlPrefix(e.link.split("=")(1))).map(s => (e, s))))
      pricesWithPriceLists = pricesWithDetails.map((changeLinkToPriceList _).tupled).filter(_.isDefined).map(_.get)
      pricesWithLinkedPrices <- Future.sequence(pricesWithPriceLists.map(e => retriever(e.link).map(s => (e, Jsoup.parse(s).getElementsByClass("gpc-lista-a").toSeq))))
      finalPrices = pricesWithLinkedPrices.flatMap(p => p._2.map(getPrice(p._1)))
    } yield {
      finalPrices
    }
  }

  def getSuggestions(query: String, tables: Tables, retriever: (String) => Future[String])(implicit exec: ExecutionContext): Future[Seq[PartialPrice]] = {
    for {
      complete <- retriever(autoCompleteUrl(query))
    } yield {
      Jsoup.parse(complete).getElementsByTag("a").toList.map(e => PartialPrice(e.text(), e.attr("href"), None))
    }
  }
}
