package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import services.GameOn.GameOn

import scala.concurrent.{ExecutionContext, Future}


case class UrlAddress(url: String, cookies : Option[String])

case class GameEntry(name: String, similarities : EntrySimilarity)

case class EntrySimilarity(same : Seq[(Long, GameOn)]) {
  def this(entry: GogEntry) = this(Seq((entry.gogId, GameOn.Gog)))

  def this(entry: SteamEntry) = this(Seq((entry.steamId, GameOn.Steam)))

  def add(entry: GogEntry) : EntrySimilarity = EntrySimilarity(same :+ (entry.gogId, GameOn.Gog))

  def add(entry: SteamEntry) : EntrySimilarity = EntrySimilarity(same :+ (entry.steamId, GameOn.Steam))

  def merge(similarity : EntrySimilarity) : EntrySimilarity = EntrySimilarity(same ++ similarity.same)
}

object GameOn extends Enumeration {
  type GameOn = Value
  val Gog,Steam = Value
}

object GameEntry{

  implicit val gameWrites: Writes[GameEntry] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "on").write[Seq[String]]
    ) (unlift(GameEntry.unpackToJson))

  def unpackToJson(e : GameEntry) : Option[(String, Seq[String])] = {
    Some(e.name, e.similarities.same.map(x => x._2.toString))
  }

  def generateFromNames(tables : Tables)(implicit ec: ExecutionContext) : Future[Seq[GameEntry]]= {
    def merge(entries : Seq[GameEntry]) : EntrySimilarity = entries.map(e => e.similarities).fold(EntrySimilarity(Seq()))((s1, s2) => s1.merge(s2))

    for{
      gog <- tables.getGogEntries.map(_.map(e => GameEntry(e.title, new EntrySimilarity(e))))
      steam <- tables.getSteamEntries.map(_.map(e => GameEntry(e.name, new EntrySimilarity(e))))
    } yield {
       (gog ++ steam).groupBy(_.name).toSeq.map(e => GameEntry(e._1, merge(e._2))).sortBy(_.name)
    }
  }
}

//TODO : Duplicate entries - more states during merge, table for reverse mapping in case of duplicates - High - show ids? filter out for same source lists entries that have same ids on left and right?
//TODO : column filters - high
//TODO : DLC - eliminate entries, move to separate table? - low
//TODO : case sensitivity - fix the entries by upper casing? - low
