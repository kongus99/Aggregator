package model

import javax.inject._

import play.api.db.slick.DatabaseConfigProvider
import services.{GogEntry, SteamEntry}
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

  def replaceGogData(data : List[GogEntry]) : Future[Seq[GogEntry]] =
    db.run(gogData.delete andThen (gogData ++= data) andThen gogData.result)

  def replaceSteamData(data : List[SteamEntry]) : Future[Seq[SteamEntry]] =
    db.run(steamData.delete andThen (steamData ++= data) andThen steamData.result)

  lazy val get = {
    def start() = {
      val initialization = MTable.getTables.flatMap(tables => {
        val areCreated = tables.exists(t => t.name.name == "GOG_DATA")
        if(areCreated) DBIO.successful(1) else (gogData.schema ++ steamData.schema).create
      })
      Await.result(db.run(initialization), Duration.Inf)
    }
    start()
  }
  get
}
