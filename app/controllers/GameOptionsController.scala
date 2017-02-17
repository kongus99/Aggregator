package controllers

import javax.inject.{Inject, Singleton}

import model.Tables
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, AnyContent, Controller}
import services.PriceHost.{PriceHost => _, _}
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GameOptionsController @Inject()(tables: Tables, client: WSClient)(implicit exec: ExecutionContext) extends Controller {

  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)
  val golRetriever = new GolRetriever(client)

  def fetch(steamId: Long): Action[AnyContent] = Action.async {
    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      queries <- queries(steamEntry.name, None)
    } yield {
      Ok(Json.toJson(GameOptions(steamEntry, queries)))
    }
  }

  def changeSearch(userId: Long, selectedResult: String, site: String, steamId: Long): Action[AnyContent] = Action.async {
    Future(Ok(Json.toJson("Done.")))
  }

  def fetchSearch(userId: Long, query: String, site: String, steamId: Long): Action[AnyContent] = Action.async {

    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      queries <- queries(query, Some(site))
    } yield {
      Ok(Json.toJson(GameOptions(steamEntry, queries)))
    }
  }

  private def queries(query: String, site : Option[String]): Future[Seq[GameQuery]] = {
    for {
      keyeNames <- KeyePricesFetcher.getSuggestions(query, tables, keyeRetriever.retrieve)
      fkNames <- FKPricesFetcher.getSuggestions(query, tables, fkRetriever.retrieve)
      golNames <- GolPricesFetcher.getSuggestions(query, tables, golRetriever.retrieve)
    } yield {
      GameQuery(query, FK.toString, fkNames, "") :: GameQuery(query, Keye.toString, keyeNames, "") :: GameQuery(query, Gol.toString, golNames, "") :: Nil
    }
  }

}