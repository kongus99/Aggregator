package controllers

import javax.inject.{Inject, Singleton}

import model.Tables
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller}
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GameOptionsController @Inject()(tables: Tables)(implicit exec: ExecutionContext) extends Controller {

  def fetch(steamId: Long): Action[AnyContent] = Action.async {
    for{
      steamEntry <- tables.getSteamEntryById(steamId)
    } yield {
      val queries = PriceHost.values.map(h => GameQuery(steamEntry.name, h.toString, "result1" :: "result2" :: Nil, "result2")).toList
      Ok(Json.toJson(GameOptions(steamEntry, queries)))
    }
  }

  def changeSearch(userId: Long, selectedResult : String, site : String, steamId : Long) = Action.async {
    Future(Ok(Json.toJson("Done.")))
  }

  def fetchSearch(userId: Long, query : String, site : String, steamId : Long) = Action.async {

    for{
      steamEntry <- tables.getSteamEntryById(steamId)
    } yield {
      val queries = PriceHost.values.map(h => GameQuery(steamEntry.name, h.toString, "new 1" :: "new 2" :: "new 2" :: Nil, "")).toList
      Ok(Json.toJson(GameOptions(steamEntry, queries)))
    }
  }
}