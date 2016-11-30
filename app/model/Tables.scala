package model

import javax.inject._

import controllers.MatchEntry
import play.api.db.slick.DatabaseConfigProvider
import services.GameOn._
import services.{GameOn, GogEntry, SteamEntry}
import slick.driver.JdbcProfile
import slick.jdbc.meta.MTable
import slick.lifted.ProvenShape

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

@Singleton
class Tables @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit exec: ExecutionContext) {


  val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig.driver.api._

  val db = dbConfig.db

  val gogData = TableQuery[GogData]
  val steamData = TableQuery[SteamData]
  val matchData = TableQuery[MatchData]

  class GogData(tag: Tag) extends Table[GogEntry](tag, "GOG_DATA") {
    def title = column[String]("GOG_DATA_TITLE")

    def gogId = column[Long]("GOG_DATA_GOG_ID", O.PrimaryKey)

    def onWishList = column[Boolean]("GOG_DATA_WISH_LIST")

    def * : ProvenShape[GogEntry] = {

      val apply: (String, Long, Boolean) => GogEntry = (name, gogId, onWishList) => new GogEntry(name, gogId, onWishList)

      val unapply: (GogEntry) => Option[(String, Long, Boolean)] = g => Some((g.title, g.gogId, g.onWishList))
      (title, gogId, onWishList) <>(apply.tupled, unapply)
    }
  }

  class SteamData(tag: Tag) extends Table[SteamEntry](tag, "STEAM_DATA") {
    def name = column[String]("STEAM_DATA_NAME")

    def steamId = column[Long]("STEAM_DATA_STEAM_ID", O.PrimaryKey)

    def onWishList = column[Boolean]("STEAM_DATA_WISH_LIST")

    def * : ProvenShape[SteamEntry] = {

      val apply: (String, Long, Boolean) => SteamEntry = (name, steamId, onWishList) => new SteamEntry(name, steamId, onWishList)

      val unapply: (SteamEntry) => Option[(String, Long, Boolean)] = g => Some((g.name, g.steamId, g.onWishList))
      (name, steamId, onWishList) <>(apply.tupled, unapply)
    }
  }

  class MatchData(tag: Tag) extends Table[MatchEntry](tag, "MATCH_DATA") {
    def leftOn = column[String]("MATCH_LEFT_ON")

    def rightOn = column[String]("MATCH_RIGHT_ON")

    def leftId = column[Long]("MATCH_LEFT_ID")

    def rightId = column[Long]("MATCH_RIGHT_ID")

    def allUnique = primaryKey("MATCH_DATA_ALL_UNIQUE", (leftOn, rightOn, leftId, rightId))

    override def * : ProvenShape[MatchEntry] = {

      val apply: (String, String, Long, Long) => MatchEntry =
        (leftOn, rightOn, leftId, rightId) => MatchEntry(GameOn.withName(leftOn), GameOn.withName(rightOn), leftId, rightId)

      val unapply: MatchEntry => Option[(String, String, Long, Long)] =
        e => Some(e.leftOn.toString, e.rightOn.toString, e.leftId, e.rightId)

      (leftOn, rightOn, leftId, rightId) <> (apply.tupled, unapply)
    }
  }

  def getAllMatches : Future[Map[(GameOn, GameOn), Set[(Long, Long)]]] = {
    for{
      matches <- db.run(matchData.result)
    }
    yield {
      val originalMatches = matches.groupBy(e => (e.leftOn, e.rightOn)).mapValues(_.map(e => (e.leftId, e.rightId)).toSet)
      val reflexiveMatches = originalMatches.map(e => (e._1.swap, e._2.map(_.swap)))
      (originalMatches.keySet ++ reflexiveMatches.keySet).map(k => (k,reflexiveMatches.getOrElse(k, Set()) ++ originalMatches.getOrElse(k, Set()))).toMap
    }
  }

  def changeMatch(e: MatchEntry): Future[Boolean] = {
    val condition: (MatchData) => Rep[Boolean] =
      d => (d.leftOn === e.leftOn.toString && d.rightOn === e.rightOn.toString && d.leftId === e.leftId && d.rightId === e.rightId) ||
           (d.rightOn === e.leftOn.toString && d.leftOn === e.rightOn.toString && d.rightId === e.leftId && d.leftId === e.rightId)
    def deleteRow() = db.run(matchData.filter(condition).delete).map(_ => false)
    def insertRow() = db.run(matchData += e).map(_ => true)

    getAllMatches.flatMap(allMatches => {
      val key = (e.leftOn, e.rightOn)
      val value = (e.leftId, e.rightId)
      allMatches.get(key).map(s => s.contains(value)).collect({
        case true => deleteRow()
        case false => insertRow()
      }).getOrElse(insertRow())
    })


  }

  def replaceGogData(data : Seq[GogEntry]) =
    db.run(gogData.delete andThen (gogData ++= data))

  def getGogEntries(sources : Option[Boolean]) : Future[Seq[GogEntry]] = {
    val query = sources.map(s => gogData.filter(e => e.onWishList === s)).getOrElse(gogData)
    db.run(query.result)
  }

  def getSteamEntries(sources : Option[Boolean]) : Future[Seq[SteamEntry]] = {
    val query = sources.map(s => steamData.filter(e => e.onWishList === s)).getOrElse(steamData)
    db.run(query.result)
  }

  def replaceSteamData(data : Seq[SteamEntry]) =
    db.run(steamData.delete andThen (steamData ++= data))

  lazy val get = {
    def start() = {
      val initialization = MTable.getTables.flatMap(tables => {
        val areCreated = tables.exists(t => t.name.name == "GOG_DATA")
        if(areCreated) DBIO.successful(1) else (gogData.schema ++ steamData.schema ++ matchData.schema).create
      })
      Await.result(db.run(initialization), Duration.Inf)
    }
    start()
  }
  get
}
