package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads}
import services.GameEntry._

import scala.concurrent.{ExecutionContext, Future}

case class SteamEntry(id : Option[Long], name : String, steamId : Long)

object SteamEntry{
  val regExp = "var rgGames = (.+);".r

  implicit val gogReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) ((n, i) => SteamEntry(None, n, i))

  def getFromSteam(tables : Tables)(data: String)(implicit exec: ExecutionContext): Future[List[GameEntry]] = {
    val parsed = Json.parse(regExp.findAllMatchIn(data).map(m => m.group(1)).next()).validate[List[SteamEntry]].get
    tables.replaceSteamData(parsed).map(p => generateFromNames(currentGogData, p.map(_.name).toList))
  }

}
