package model

import javax.inject._

import controllers.MatchEntry
import play.api.db.slick.DatabaseConfigProvider
import services.GameOn._
import services.{GameOn, GogEntry, SteamEntry}
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile
import slick.jdbc.meta.MTable
import slick.lifted.ProvenShape

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

@Singleton
class Tables @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit exec: ExecutionContext) {


  val dbConfig: DatabaseConfig[JdbcProfile] = dbConfigProvider.get[JdbcProfile]

  import dbConfig.driver.api._

  val db = dbConfig.db

  val steamData = TableQuery[SteamData]
  val matchData = TableQuery[MatchData]
  val userData = TableQuery[UserData]
  val gogData = TableQuery[GogData]
  val gogOwnershipData = TableQuery[GogOwnershipData]

  class UserData(tag: Tag) extends Table[User](tag, "USER_DATA") {
    def id = column[Long]("USER_DATA_ID", O.PrimaryKey, O.AutoInc)

    def gogLogin = column[Option[String]]("USER_DATA_GOG_LOGIN")

    def steamLogin = column[Option[String]]("USER_DATA_STEAM_LOGIN")

    def gogLoginUnique = index("USER_DATA_GOG_LOGIN_UNIQUE", gogLogin, unique = true)

    def steamLoginUnique = index("USER_DATA_STEAM_LOGIN_UNIQUE", steamLogin, unique = true)

    def * : ProvenShape[User] = (id.?, steamLogin, gogLogin) <> (User.tupled, User.unapply)
  }

  class GogData(tag: Tag) extends Table[(Long, String)](tag, "GOG_DATA") {
    def gogId = column[Long]("GOG_DATA_GOG_ID", O.PrimaryKey)

    def title = column[String]("GOG_DATA_TITLE")

    def * : ProvenShape[(Long, String)] = {

      val apply: (Long, String) => (Long, String) = (gogId, title) => (gogId, title)

      val unapply: ((Long, String)) => Option[(Long, String)] = g => Some(g._1, g._2)
      (gogId, title) <>(apply.tupled, unapply)
    }

  }

  class GogOwnershipData(tag: Tag) extends Table[(Long, Long, Option[BigDecimal], Option[BigDecimal])](tag, "GOG_OWNERSHIP_DATA") {
    def gogId = column[Long]("GOG_OWNERSHIP_DATA_GOG_ID")

    def userId = column[Long]("GOG_OWNERSHIP_DATA_USER_ID")

    def price = column[Option[BigDecimal]]("GOG_OWNERSHIP_DATA_PRICE", O.SqlType("DECIMAL(6,2)"))

    def discountedPrice = column[Option[BigDecimal]]("GOG_OWNERSHIP_DATA_PRICE_DISCOUNTED", O.SqlType("DECIMAL(6,2)"))

    def comboUnique = primaryKey("GOG_OWNERSHIP_DATA_COMBO_UNIQUE", (gogId, userId))

    def gogFk = foreignKey("GOG_DATA_FK", gogId, gogData)(_.gogId, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def userFk = foreignKey("USER_DATA_FK", userId, userData)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def * : ProvenShape[(Long, Long, Option[BigDecimal], Option[BigDecimal])] = {

      val apply: (Long, Long, Option[BigDecimal], Option[BigDecimal]) => (Long, Long, Option[BigDecimal], Option[BigDecimal]) =
        (gogId, userId, price, discountedPrice) => (gogId, userId, price, discountedPrice)

      val unapply: ((Long, Long, Option[BigDecimal], Option[BigDecimal])) => Option[(Long, Long, Option[BigDecimal], Option[BigDecimal])] =
        g => Some(g._1, g._2, g._3, g._4)
      (gogId, userId, price, discountedPrice) <>(apply.tupled, unapply)
    }
  }

  class SteamData(tag: Tag) extends Table[SteamEntry](tag, "STEAM_DATA") {
    def name = column[String]("STEAM_DATA_NAME")

    def steamId = column[Long]("STEAM_DATA_STEAM_ID", O.PrimaryKey)

    def price = column[Option[BigDecimal]]("STEAM_DATA_PRICE", O.SqlType("DECIMAL(6,2)"))

    def discountedPrice = column[Option[BigDecimal]]("STEAM_DATA_PRICE_DISCOUNTED", O.SqlType("DECIMAL(6,2)"))

    def * : ProvenShape[SteamEntry] = {

      val apply: (String, Long, Option[BigDecimal], Option[BigDecimal]) => SteamEntry = (name, steamId, price, discounted) => new SteamEntry(name, steamId, price, discounted)

      val unapply: (SteamEntry) => Option[(String, Long, Option[BigDecimal], Option[BigDecimal])] = g => Some((g.name, g.steamId, g.price, g.discounted))
      (name, steamId, price, discountedPrice) <>(apply.tupled, unapply)
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

  def getUserBySteamLogin(login : Option[String]) : Future[Option[User]] =
    db.run(userData.filter(_.steamLogin === login).result.headOption)

  def replaceGogData(user : Option[User], data : Seq[GogEntry]): Future[_] =
    user.map(u => {
      val ids = data.map(_.gogId).toSet
      val newOwnership = data.map(g => (g.gogId, u.id.get, g.price, g.discounted))
      def oldDataIdsQuery =
        gogData.filter(_.gogId.inSet(ids)).map(_.gogId)
      def insertNewData(oldDataIds : Seq[Long]) = {
        val newData = data.filter(d => !oldDataIds.contains(d.gogId)).map(d => (d.gogId, d.title))
        gogData ++= newData
      }
      lazy val  deleteOldOwnership = gogOwnershipData.filter(_.userId === u.id.get).delete
      lazy val  addNewOwnership = gogOwnershipData ++= newOwnership
      db.run((oldDataIdsQuery.result.flatMap(insertNewData) >> deleteOldOwnership >> addNewOwnership).transactionally)
    }).getOrElse(Future{true})

  def getGogEntries(user : Option[User], sources : Option[Boolean]) : Future[Seq[GogEntry]] = {
    for {rows <- user.map(u => {
      db.run(gogOwnershipData.filter(e => e.price.isDefined === sources && e.userId === u.id.get).join(gogData).on(_.gogId === _.gogId).result)
    }).getOrElse(
      db.run(gogOwnershipData.join(gogData).on(_.gogId === _.gogId).result)
    )} yield {
      rows.map(pair => GogEntry(pair._2._2, pair._2._1, pair._1._3, pair._1._4))
    }
  }

  def getSteamEntries(sources : Option[Boolean]) : Future[Seq[SteamEntry]] = {
    val query = sources.map(s => steamData.filter(e => e.price.isDefined === s)).getOrElse(steamData)
    db.run(query.result)
  }

  def replaceSteamData(data : Seq[SteamEntry]): Future[_] =
    db.run(steamData.delete andThen (steamData ++= data))

  lazy val get: AnyVal = {
    def start() = {
      val initialization = MTable.getTables.flatMap(tables => {
        val areCreated = tables.exists(t => t.name.name == "GOG_DATA")
        if(areCreated)
          DBIO.successful(1)
        else
          userData.schema.create andThen
            gogData.schema.create andThen
            steamData.schema.create andThen
            matchData.schema.create andThen
            gogOwnershipData.schema.create andThen
            (userData += User(None, Some("kongus"), Some("kongus99")))
      })
      Await.result(db.run(initialization), Duration.Inf)
    }
    start()
  }
  get
}
