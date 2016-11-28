package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import services.GameOn.GameOn

import scala.concurrent.{ExecutionContext, Future}


case class UrlAddress(url: String, cookies: Option[String])

case class GameEntry(gog: Seq[GogEntry], steam: Seq[SteamEntry]) {
  val name: String = gog.headOption.map(_.title).getOrElse(steam.head.name)
}

object GameOn extends Enumeration {
  type GameOn = Value
  val Gog, Steam = Value
}

object GameEntry {

  implicit val gameWrites: Writes[GameEntry] = (
    (JsPath \ "gog").write[Seq[GogEntry]] and
      (JsPath \ "steam").write[Seq[SteamEntry]]
    ) (unlift(GameEntry.unpackToJson))

  def unpackToJson(e: GameEntry): Option[(Seq[GogEntry], Seq[SteamEntry])] = {
    Some((e.gog, e.steam))
  }

  def generateFromNames(tables: Tables)(implicit ec: ExecutionContext): Future[Seq[GameEntry]] = {
    def simplify(p: ((GameOn, Long), (GameOn, Long))) = if (p._1._1 == GameOn.Gog || (p._1._1 == GameOn.Steam && p._2._1 == GameOn.Steam && p._1._2 < p._2._2)) p.swap else p


    for {
      gog <- tables.getGogEntries
      steam <- tables.getSteamEntries
      matches <- tables.getAllMatches
    } yield {
      val gogMap = gog.map(e => (e.gogId, e)).toMap
      val steamMap = steam.map(e => (e.steamId, e)).toMap

      val simplified = matches.toSeq.flatMap(m => m._2.map(p => ((m._1._1, p._1), (m._1._2, p._2)))).map(simplify).distinct
      val mappedBySteamId = simplified.map(p => (p._1._2, p._2)).groupBy(_._1).mapValues(_.map(_._2))
      val repeated = mappedBySteamId.toSeq.map(p => (GameOn.Steam, p._1) +: p._2).map(s => s.partition(_._1 == GameOn.Steam)).map(p => GameEntry(p._2.map(e => gogMap(e._2)), p._1.map(e => steamMap(e._2))))
      val repeatingGogIds = matches.filter(p => p._1._1 == GameOn.Gog).flatMap(_._2).keys.toSet
      val repeatingSteamIds = matches.filter(p => p._1._1 == GameOn.Steam).flatMap(_._2).keys.toSet
      val onlyGog = gog.filter(g => !repeatingGogIds.contains(g.gogId)).map(g => GameEntry(Seq(g), Seq()))
      val onlySteam = steam.filter(g => !repeatingSteamIds.contains(g.steamId)).map(s => GameEntry(Seq(), Seq(s)))
      (repeated ++ onlyGog ++ onlySteam).sortBy(_.name)
    }
  }
}

//TODO : column filters - high
//TODO : DLC - eliminate entries, move to separate table? - low
//TODO : case sensitivity - fix the entries by upper casing? - low
