package services

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}


case class UrlAddress(url: String, cookies : Option[String])

case class GameEntry(name: String, onGog: Boolean, onSteam: Boolean)

object GameEntry{

  var currentGogData :List[String]= Nil

  var currentSteamData : List[String] = Nil

  implicit val gameWrites: Writes[GameEntry] = (
    (JsPath \ "name").write[String] and
      (JsPath \ "onGog").write[Boolean] and
      (JsPath \ "onSteam").write[Boolean]
    ) (unlift(GameEntry.unapply))

  def generateFromNames(gogNames : List[String], steamNames : List[String]) : List[GameEntry]= {
    val gog = gogNames.map(GameEntry(_, onGog = true, onSteam = false))
    val steam = steamNames.map(GameEntry(_, onGog = false, onSteam = true))
    gog ::: steam

  }
}

