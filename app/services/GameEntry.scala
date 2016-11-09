package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import services.GameOn.GameOn

import scala.concurrent.{ExecutionContext, Future}


case class UrlAddress(url: String, cookies : Option[String])

case class GameEntry(name: String, on : Seq[GameOn])


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
    Some(e.name, e.on.map(x => x.toString))
  }

  def generateFromNames(tables : Tables)(implicit ec: ExecutionContext) : Future[Seq[GameEntry]]= {
//    println(gogNames.flatMap(g => steamNames.map(s => (g, s))).map(p => (p._1, p._2, ThresholdLevenshtein.count(p._1, p._2, 2))).filter(t => t._3 == 1))
    def merge(entries : Seq[GameEntry]) : Seq[GameOn] = entries.flatMap(e => e.on)

    for{
      gog <- tables.getGogEntries.map(_.map(e => GameEntry(e.title, GameOn.Gog ::Nil)))
      steam <- tables.getSteamEntries.map(_.map(e => GameEntry(e.name, GameOn.Steam :: Nil)))
    } yield {
       (gog ++ steam).groupBy(_.name).toSeq.map(e => GameEntry(e._1, merge(e._2))).sortBy(_.name)
    }
  }
}

//TODO : Different names - editing distance, manual matching, table for matches - high
//TODO : Duplicate entries - more states during merge, table for reverse mapping in case of duplicates - High
//TODO : DLC - eliminate entries, move to separate table? - low
//TODO : case sensitivity - fix the entries by upper casing? - low
