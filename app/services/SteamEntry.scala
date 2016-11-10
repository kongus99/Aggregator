package services

import model.Tables
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import services.GameEntry._

import scala.concurrent.{ExecutionContext, Future}

case class SteamEntry(id : Option[Long], name : String, steamId : Long)

object SteamEntry{
  val regExp = "var rgGames = (.+);".r

  implicit val steamReads: Reads[SteamEntry] = ((JsPath \ "name").read[String] and (JsPath \ "appid").read[Long]) ((n, i) => SteamEntry(None, n, i))
  implicit val steamWrites: Writes[SteamEntry] = (
    (JsPath \ "id").write[Long] and
      (JsPath \ "name").write[String] and
      (JsPath \ "steamId").write[Long])((e) => (e.id.getOrElse(0), e.name, e.steamId))

  def getFromSteam(tables : Tables)(data: String)(implicit exec: ExecutionContext): Future[Seq[GameEntry]] = {
    val parsed = Json.parse(regExp.findAllMatchIn(data).map(m => m.group(1)).next()).validate[List[SteamEntry]].get
    tables.replaceSteamData(parsed).flatMap(r => generateFromNames(tables))
  }

}
