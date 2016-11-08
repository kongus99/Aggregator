package services

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import services.EntryConvergence.EntryConvergence


case class UrlAddress(url: String, cookies : Option[String])

case class GameEntry(name: String, convergence : EntryConvergence)


object EntryConvergence extends Enumeration {
  type EntryConvergence = Value
  val Gog, Steam, Both = Value
}

object GameEntry{

  var currentGogData :List[String]= Nil

  var currentSteamData : List[String] = Nil

  implicit val gameWrites: Writes[GameEntry] = (
    (JsPath \ "name").write[String] and
      (JsPath \ "convergence").write[String]
    ) (unlift(GameEntry.unpackToJson))

  def unpackToJson(x : GameEntry) : Option[(String, String)] = Some(x.name, x.convergence.toString)

  def generateFromNames(gogNames : List[String], steamNames : List[String]) : List[GameEntry]= {
    val gog = gogNames.map(GameEntry(_, EntryConvergence.Gog))
    val steam = steamNames.map(GameEntry(_, EntryConvergence.Steam))
    gog ::: steam
  }
}

