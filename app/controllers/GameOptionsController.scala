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
      val go = GameOptions(steamEntry, GameQuery("some query", "some site", "result1" :: "result2" :: Nil, "result2") :: Nil)
      Ok(Json.toJson(go))
    }
  }

  def changeSearch(userId: Long, selectedResult : String, site : String, steamId : Long) = Action.async {
    Future(Ok(Json.toJson("Done.")))
  }

  def fetchSearch(userId: Long, query : String, site : String, steamId : Long) = Action.async {

    for{
      steamEntry <- tables.getSteamEntryById(steamId)
    } yield {
      val go = GameOptions(steamEntry, GameQuery(query, site, "new 1" :: "new 2" :: "new 2" :: Nil, "") :: Nil)
      Ok(Json.toJson(go))
    }
  }
}