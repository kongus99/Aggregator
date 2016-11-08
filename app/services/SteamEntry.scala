package services

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads}
import services.GameEntry._

case class SteamEntry(name : String, id : Long)

object SteamEntry{
  val regExp = "var rgGames = (.+);".r

  implicit val gogReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) (SteamEntry.apply _)

  def getFromSteam(data: String): List[GameEntry] = {
    generateFromNames(currentGogData, parseSteamNames(data))
  }

  def parseSteamNames(body: String): List[String] = {
    val steamJson = regExp.findAllMatchIn(body).map(m => m.group(1)).next()
    currentSteamData = Json.parse(steamJson).validate[List[SteamEntry]].get.map(_.name)
    currentSteamData
  }
}
