package services

import model.Tables
import org.jsoup.Jsoup

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class MuveEntry(steamId: Long, price: Option[Float] = None, discounted: Option[Float] = None, muveId: String, muveName: String)

case class MuveChoice(steamEntry: SteamEntry, muveEntries: Seq[MuveEntry])

object MuveChoice {

  import scala.collection.JavaConversions._

  def getChoices(tables: Tables)(retriever: String => Future[(String, String)])(implicit exec: ExecutionContext): Future[Seq[MuveChoice]] = {

    def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({ case e => Failure(e) })

    def convertToPrice(e: String): Float = e.split(" ")(0).replaceAll(",", ".").toFloat

    def parsePage(q: String, p: String, steamMap: Map[String, SteamEntry]): MuveChoice = {
      val entry = steamMap(q)
      val muveEntries = Jsoup.parse(p).body().getElementsByAttribute("data-productid").toList.filter(e => e.hasClass("platform1")).map(pc => {
        val id = pc.attr("data-productid")
        val currentPrice = convertToPrice(pc.attr("data-price"))
        val prePromoPrice = pc.getElementsByClass("prod-aval promo").headOption.map(e => convertToPrice(e.text()))
        val (price, discounted) = if (prePromoPrice.isDefined) (prePromoPrice, Some(currentPrice)) else (Some(currentPrice), None)
        val name = pc.getElementsByClass("prod-title").head.text()
        MuveEntry(entry.steamId, price, discounted, id, name)
      })
      MuveChoice(entry, muveEntries)
    }

    for {
      entries <- tables.getSteamEntries(Some(true))
      pages <- Future.sequence(entries.map(e => retriever(e.name)).map(futureToFutureTry))
    } yield {
      val steamEntryMap = entries.map(e => (e.name, e)).toMap
      pages.map(_.getOrElse(("", ""))).filter(!_._2.isEmpty).map(p => (p._1, p._2, steamEntryMap))
    }.map((parsePage _).tupled).filter(_.muveEntries.nonEmpty)

  }

}
