package services

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import services.GameOn.GameOn


case class UrlAddress(url: String, cookies : Option[String])

case class GameEntry(name: String, on : List[GameOn])


object GameOn extends Enumeration {
  type GameOn = Value
  val Gog,Steam = Value
}

object GameEntry{

  var currentGogData :List[String]= Nil

  var currentSteamData : List[String] = Nil

  implicit val gameWrites: Writes[GameEntry] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "on").write[List[String]]
    ) (unlift(GameEntry.unpackToJson))

  def unpackToJson(e : GameEntry) : Option[(String, List[String])] = {
    Some(e.name, e.on.map(x => x.toString))
  }

  def generateFromNames(gogNames : List[String], steamNames : List[String]) : List[GameEntry]= {
    println(gogNames.flatMap(g => steamNames.map(s => (g, s))).map(p => (p._1, p._2, ThresholdLevenshtein.count(p._1, p._2, 2))).filter(t => t._3 == 1))

    val gog = gogNames.map(GameEntry(_, GameOn.Gog ::Nil))
    val steam = steamNames.map(GameEntry(_, GameOn.Steam :: Nil))
    def merge(entries : List[GameEntry]) : List[GameOn] = entries.flatMap(e => e.on)
    (gog ::: steam).groupBy(_.name).toList.map(e => GameEntry(e._1, merge(e._2))).sortBy(_.name)
  }
}

//TODO : Different names - editing distance, manual matching, table for matches - high
//TODO : Duplicate entries - more states during merge, table for reverse mapping in case of duplicates - High
//TODO : DLC - eliminate entries, move to separate table? - low
//TODO : case sensitivity - fix the entries by upper casing? - low
