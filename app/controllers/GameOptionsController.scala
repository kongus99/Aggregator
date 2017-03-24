package controllers

import javax.inject.{Inject, Singleton}

import model.Tables
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, AnyContent, Controller}
import services.PriceHost._
import services._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GameOptionsController @Inject()(tables: Tables, client: WSClient)(implicit exec: ExecutionContext) extends Controller {

  val fkRetriever = new FKRetriever(client)
  val keyeRetriever = new KeyeRetriever(client)
  val golRetriever = new GolRetriever(client)

  def fetch(userId: Long, steamId: Long): Action[AnyContent] = Action.async {
    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      (queries, existing) <- getQueries(userId, Seq(), steamEntry, tables)
      queryResults <- getQueryResults(steamEntry, queries)
      response = constructResponse(steamEntry, queries, queryResults, existing, None)
    } yield {
      Ok(Json.toJson(GameOptions(steamEntry, response)))
    }
  }

  def changeSearch(userId: Long, steamId: Long, site: String, selectedResult: Option[String]): Action[AnyContent] = Action.async {
    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      _ <- tables.changeQueryData(userId, steamId, GameQuery(steamEntry.name, site, Seq(), selectedResult), Right(selectedResult))
    } yield {
      Ok(Json.toJson(""))
    }
  }

  private def constructResponse(steamEntry: SteamEntry, queries: Map[String, String], queryResults: Map[String, Seq[String]], existing : Seq[GameQuery], newQuery : Option[GameQuery]): Seq[GameQuery] = {

    val fixedQueries : Map[String, GameQuery] =
      (newQuery.map(Seq(_)).getOrElse(Seq()) ++ existing).reverse.map(q => (q.site,q.copy(allResults = queryResults(q.site)))).toMap

    val defaultQueries: Map[String, GameQuery] = {
      def entry(site: String) = {
        (site, GameQuery(queries(site), site, queryResults(site), BestMatcher.chooseBestMatch(steamEntry.name, queryResults(site), identity[String])))
      }

      (entry(Keye.toString) :: entry(FK.toString) :: entry(Gol.toString) :: Nil).toMap
    }

    (defaultQueries ++ fixedQueries).values.toSeq
  }

  def fetchSearch(userId: Long, steamId: Long, query: String, site: String): Action[AnyContent] = Action.async {
    for {
      steamEntry <- tables.getSteamEntryById(steamId)
      (queries, existing) <- getQueries(userId, Seq((site, query)), steamEntry, tables)
      queryResults <- getQueryResults(steamEntry, queries)
      bestMatch =  BestMatcher.chooseBestMatch(steamEntry.name, queryResults(site), identity[String])
      newQuery = GameQuery(query, site, queryResults(site), bestMatch)
      _ <- tables.changeQueryData(userId, steamId, newQuery, Left(query))
      response = constructResponse(steamEntry, queries, queryResults, existing, Some(newQuery))
    } yield {
      Ok(Json.toJson(GameOptions(steamEntry, response)))
    }
  }

  private def getQueries(userId: Long, newQueries : Seq[(String, String)], steamEntry: SteamEntry, tables: Tables): Future[(Map[String, String], Seq[GameQuery])] = {
    val defaultQueries = (Keye.toString, steamEntry.name) :: (FK.toString, steamEntry.name) :: (Gol.toString, steamEntry.name) :: Nil

    for {
      existing <- tables.getQueryData(userId, steamEntry.steamId)
    } yield {
      val existingQueries = existing.map(q => (q.site, q.query))
      ((defaultQueries ++ existingQueries ++ newQueries).toMap, existing)
    }


  }

  private def getQueryResults(steamEntry: SteamEntry, queries: Map[String,String]): Future[Map[String, Seq[String]]] = {

    for {
      keyeNames <- KeyePricesFetcher.getSuggestions(queries(Keye.toString), tables, keyeRetriever.retrieve)
      fkNames <- FKPricesFetcher.getSuggestions(queries(FK.toString), tables, fkRetriever.retrieve)
      golNames <- GolPricesFetcher.getSuggestions(queries(Gol.toString), tables, golRetriever.retrieve)
    } yield {
      ((FK.toString, fkNames.map(_.name)) :: (Keye.toString, keyeNames.map(_.name)) :: (Gol.toString, golNames.map(_.name)) :: Nil).toMap
    }

  }

  def refresh(userId: Long, steamId: Long): Action[AnyContent] =  Action.async {
    Future(Ok("Scheduled refresh"))
  }

}