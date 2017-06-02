package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import services.GameOn.GameOn
import services.GameSources.GameSources

import scala.concurrent.{ExecutionContext, Future}


case class UrlAddress(url: String, headers : Seq[(String, String)] = Seq())

trait ShopEntry {
  def id: Long
  def title: String
  def link : String
  def price: Option[BigDecimal]
  def discounted: Option[BigDecimal]
  def owned : Boolean
  def genres : String
}

case class GameEntry(gog: Seq[GogEntry], steam: Seq[SteamEntry], prices : Seq[PriceEntry] = Seq()) {
  val name: String = gog.headOption.map(_.title).getOrElse(steam.head.name)
}

object GameOn extends Enumeration {
  type GameOn = Value
  val Gog, Steam = Value
}

object GameSources extends Enumeration {
  type GameSources = Value
  val Owned, WishList, Both = Value

  def toOption(value: Value) : Option[Boolean] = {
    if(value == Owned) Some(true)
    else if(value == WishList) Some(false)
    else None
  }
}

object GameEntry {

  implicit val gameWrites: Writes[GameEntry] = (
    (JsPath \ "gog").write[Seq[GogEntry]] and
      (JsPath \ "steam").write[Seq[SteamEntry]] and
      (JsPath \ "prices").write[Seq[PriceEntry]]
    ) ((e) => (e.gog, e.steam, e.prices))

  private def generateFromNames(userId : Option[Long], sources  : GameSources.GameSources, tables: Tables)(implicit ec: ExecutionContext): Future[Seq[GameEntry]] = {
    def simplify(p: ((GameOn, Long), (GameOn, Long))) = if (p._1._1 == GameOn.Gog || (p._1._1 == GameOn.Steam && p._2._1 == GameOn.Steam && p._1._2 < p._2._2)) p.swap else p


    def toGameEntry(gogMap: Map[Long, GogEntry], steamMap: Map[Long, SteamEntry])(p: (Seq[(GameOn, Long)], Seq[(GameOn, Long)])): Option[GameEntry] = {
      val steam = p._1.flatMap(e => steamMap.get(e._2))
      val gog = p._2.flatMap(e => gogMap.get(e._2))
      if(gog.isEmpty && steam.isEmpty) None else Some(GameEntry(gog, steam))
    }

    for {
      gog <- tables.getGogEntries(userId, GameSources.toOption(sources)).map(_.map(e => if(e.owned) e.copy(price = None, discounted = None) else e))
      steam <- tables.getSteamEntries(userId, GameSources.toOption(sources)).map(_.map(e => if(e.owned) e.copy(price = None, discounted = None) else e))
      matches <- tables.getAllMatches
    } yield {
      val gogMap = gog.map(e => (e.gogId, e)).toMap
      val steamMap = steam.map(e => (e.steamId, e)).toMap

      val simplified = matches.toSeq.flatMap(m => m._2.map(p => ((m._1._1, p._1), (m._1._2, p._2)))).map(simplify).distinct
      val mappedBySteamId = simplified.map(p => (p._1._2, p._2)).groupBy(_._1).mapValues(_.map(_._2))
      val repeated = mappedBySteamId.toSeq.map(p => (GameOn.Steam, p._1) +: p._2).map(s => s.partition(_._1 == GameOn.Steam)).flatMap(toGameEntry(gogMap, steamMap)(_))
      val repeatingGogIds = matches.filter(p => p._1._1 == GameOn.Gog).flatMap(_._2).keys.toSet
      val repeatingSteamIds = matches.filter(p => p._1._1 == GameOn.Steam).flatMap(_._2).keys.toSet
      val onlyGog = gog.filter(g => !repeatingGogIds.contains(g.gogId)).map(g => GameEntry(Seq(g), Seq()))
      val onlySteam = steam.filter(g => !repeatingSteamIds.contains(g.steamId)).map(s => GameEntry(Seq(), Seq(s)))
      (repeated ++ onlyGog ++ onlySteam).sortBy(_.name)
    }
  }
  def gamesWithPrices(userId: Long, sources: GameSources, tables: Tables)(implicit ec: ExecutionContext): Future[Seq[GameEntry]] = {
    for {
      user <- tables.getUserById(userId)
      result <- generateFromNames(user.get.id, sources, tables)
      prices <- tables.getPrices(result.map(_.steam).filter(_.nonEmpty).flatten.filter(!_.owned)).map(_.groupBy(_.steamId).mapValues(_.sortBy(_.price)))
    } yield {
      result.map(e => if (e.steam.isEmpty) e else e.copy(prices = prices.getOrElse(e.steam.head.steamId, Seq())))
    }
  }
}

//TODO : DLC - eliminate entries, move to separate table? - low
