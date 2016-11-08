package services

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads}
import services.GameEntry._

case class GogEntry(title: String, rating : Int)

object GogEntry{

  implicit val gogReads: Reads[GogEntry] = ((JsPath \ "title").read[String] and (JsPath \ "rating").read[Int]) (GogEntry.apply _)

  def getFromGog(data: Seq[String]): Seq[GameEntry] = {
    val allNames: List[String] = data.flatMap(parseGogNames).toList
    currentGogData = allNames
    generateFromNames(allNames, currentSteamData)
  }

  def getGogPageNumber(body : String) : Int = (Json.parse(body) \ "totalPages").as[Int]

  def parseGogNames(body: String): List[String] = (Json.parse(body) \ "products").validate[List[GogEntry]].get.map(_.title)
}
