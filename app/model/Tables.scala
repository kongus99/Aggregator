package model

import javax.inject._

import play.api.db.slick.DatabaseConfigProvider
import services.GogEntry
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

  class SteamData(tag: Tag) extends Table[(Int, String, Int)](tag, "STEAM_DATA") {
    def id = column[Int]("STEAM_DATA_ID", O.PrimaryKey, O.AutoInc)

    def name = column[String]("STEAM_DATA_NAME")

    def steamId = column[Int]("STEAM_DATA_GOG_ID")

    def * = (id, name, steamId)
  }

  def replaceGogData(data : List[GogEntry]) :Future[Seq[GogEntry]] =
    db.run(gogData.delete andThen (gogData ++= data) andThen gogData.result)

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
