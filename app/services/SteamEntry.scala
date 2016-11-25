package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import services.ListEntry._

import scala.concurrent.{ExecutionContext, Future}

case class SteamEntry(name : String, steamId : Long)

object SteamEntry{
  val regExp = "var rgGames = (.+);".r

  implicit val steamReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) ((n, i) => SteamEntry(n, i))
  implicit val steamWrites: Writes[SteamEntry] = (
      (JsPath \ "name").write[String] and
      (JsPath \ "steamId").write[Long])((e) => (e.name, e.steamId))

  def getFromSteam(tables : Tables)(data: String)(implicit exec: ExecutionContext): Future[Seq[ListEntry]] = {
    val parsed = Json.parse(regExp.findAllMatchIn(data).map(m => m.group(1)).next()).validate[List[SteamEntry]].get
    tables.replaceSteamData(parsed).flatMap(r => generateFromNames(tables))
  }

}
