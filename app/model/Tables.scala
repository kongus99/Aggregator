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
    def id = column[Long]("GOG_DATA_ID", O.PrimaryKey, O.AutoInc)

    def title = column[String]("GOG_DATA_TITLE")

    def gogId = column[Long]("GOG_DATA_GOG_ID")

    def * : ProvenShape[GogEntry] = {

      val apply: (Option[Long], String, Long) => GogEntry =
        (id, name, gogId) => new GogEntry(id, name, gogId)

      val unapply: (GogEntry) => Option[(Option[Long], String, Long)] =
        g => Some((g.id, g.title, g.gogId))
      (id.?, title, gogId) <>(apply.tupled, unapply)
    }
  }

  class SteamData(tag: Tag) extends Table[SteamEntry](tag, "STEAM_DATA") {
    def id = column[Long]("STEAM_DATA_ID", O.PrimaryKey, O.AutoInc)

    def name = column[String]("STEAM_DATA_NAME")

    def steamId = column[Long]("STEAM_DATA_STEAM_ID")

    def * : ProvenShape[SteamEntry] = {

      val apply: (Option[Long], String, Long) => SteamEntry =
        (id, name, steamId) => new SteamEntry(id, name, steamId)

      val unapply: (SteamEntry) => Option[(Option[Long], String, Long)] =
        g => Some((g.id, g.name, g.steamId))
      (id.?, name, steamId) <>(apply.tupled, unapply)
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
      }).get
    })


  }

  def replaceGogData(data : List[GogEntry]) =
    db.run(gogData.delete andThen (gogData ++= data))

  def getGogEntries : Future[Seq[GogEntry]] = db.run(gogData.result)

  def getSteamEntries : Future[Seq[SteamEntry]] = db.run(steamData.result)

  def replaceSteamData(data : List[SteamEntry]) =
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
