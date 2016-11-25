package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}

import scala.concurrent.{ExecutionContext, Future}


case class UrlAddress(url: String, cookies: Option[String])

case class ListEntry(gog: Seq[GogEntry], steam: Seq[SteamEntry]){
  val name: String = gog.headOption.map(_.title).getOrElse(steam.head.name)
}

object GameOn extends Enumeration {
  type GameOn = Value
  val Gog, Steam = Value
}

object ListEntry {

  implicit val gameWrites: Writes[ListEntry] = (
    (JsPath \ "gog").write[Seq[GogEntry]] and
      (JsPath \ "steam").write[Seq[SteamEntry]]
    ) (unlift(ListEntry.unpackToJson))

  def unpackToJson(e: ListEntry): Option[(Seq[GogEntry], Seq[SteamEntry])] = {
    Some((e.gog, e.steam))
  }

  def generateFromNames(tables: Tables)(implicit ec: ExecutionContext): Future[Seq[ListEntry]] = {
    for {
      gog <- tables.getGogEntries
      steam <- tables.getSteamEntries
      matches <- tables.getAllMatches
    } yield {
      val repeatingGogIds = matches.filter(p => p._1._1 == GameOn.Gog).flatMap(_._2).keys.toSet
      val repeatingSteamIds = matches.filter(p => p._1._1 == GameOn.Steam).flatMap(_._2).keys.toSet
      val repeatingGogEntries = gog.filter(g => repeatingGogIds.contains(g.gogId))
      val repeatingSteamEntries = steam.filter(s => repeatingSteamIds.contains(s.steamId))
      val both = repeatingGogEntries.flatMap(g => repeatingSteamEntries.map(s => (g, s)))
        .filter({case (g, s) => matches.get((GameOn.Gog, GameOn.Steam)).exists(set => set.contains(g.gogId, s.steamId))})
        .map({case (g, s) => ListEntry(Seq(g), Seq(s))})
      val onlyGog = gog.filter(g => !repeatingGogIds.contains(g.gogId)).map(g => ListEntry(Seq(g), Seq()))
      val onlySteam = steam.filter(g => !repeatingSteamIds.contains(g.steamId)).map(s => ListEntry(Seq(), Seq(s)))
      (both ++ onlyGog ++ onlySteam).sortBy(_.name)
    }
  }
}

//TODO : Duplicate entries - more states during merge, table for reverse mapping in case of duplicates - High - show ids? filter out for same source lists entries that have same ids on left and right?
//TODO : column filters - high
//TODO : DLC - eliminate entries, move to separate table? - low
//TODO : case sensitivity - fix the entries by upper casing? - low
