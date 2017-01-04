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

  val matchData = TableQuery[MatchData]
  val userData = TableQuery[UserData]
  val gogData = TableQuery[GogData]
  val gogOwnershipData = TableQuery[GogOwnershipData]
  val steamData = TableQuery[SteamData]
  val steamOwnershipData = TableQuery[SteamOwnershipData]

  class UserData(tag: Tag) extends Table[User](tag, "USER_DATA") {
    def id = column[Long]("USER_DATA_ID", O.PrimaryKey, O.AutoInc)

    def gogLogin = column[Option[String]]("USER_DATA_GOG_LOGIN")

    def steamLogin = column[Option[String]]("USER_DATA_STEAM_LOGIN")

    def gogLoginUnique = index("USER_DATA_GOG_LOGIN_UNIQUE", gogLogin, unique = true)

    def steamLoginUnique = index("USER_DATA_STEAM_LOGIN_UNIQUE", steamLogin, unique = true)

    override def * : ProvenShape[User] = (id.?, steamLogin, gogLogin) <> ((User.apply _).tupled, User.unapply)
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

  class SteamData(tag: Tag) extends Table[(Long, String)](tag, "STEAM_DATA") {
    def steamId = column[Long]("STEAM_DATA_STEAM_ID", O.PrimaryKey)

    def name = column[String]("STEAM_DATA_NAME")

    def * : ProvenShape[(Long, String)] = {

      val apply: (Long, String) => (Long, String) = (steamId, name) => (steamId, name)

      val unapply: ((Long, String)) => Option[(Long, String)] = s => Some((s._1, s._2))
      (steamId, name) <>(apply.tupled, unapply)
    }
  }

  class SteamOwnershipData(tag: Tag) extends Table[(Long, Long, Option[BigDecimal], Option[BigDecimal])](tag, "STEAM_OWNERSHIP_DATA") {

    def steamId = column[Long]("STEAM_OWNERSHIP_DATA_STEAM_ID")

    def userId = column[Long]("STEAM_OWNERSHIP_DATA_USER_ID")

    def price = column[Option[BigDecimal]]("STEAM_OWNERSHIP_DATA_PRICE", O.SqlType("DECIMAL(6,2)"))

    def discountedPrice = column[Option[BigDecimal]]("STEAM_OWNERSHIP_DATA_PRICE_DISCOUNTED", O.SqlType("DECIMAL(6,2)"))

    def comboUnique = primaryKey("STEAM_OWNERSHIP_DATA_COMBO_UNIQUE", (steamId, userId))

    def steamFk = foreignKey("STEAM_DATA_FK", steamId, steamData)(_.steamId, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def userFk = foreignKey("USER_DATA_FK", userId, userData)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def * : ProvenShape[(Long, Long, Option[BigDecimal], Option[BigDecimal])] = {

      val apply: (Long, Long, Option[BigDecimal], Option[BigDecimal]) => (Long, Long, Option[BigDecimal], Option[BigDecimal]) =
        (steamId, userId, price, discountedPrice) => (steamId, userId, price, discountedPrice)

      val unapply: ((Long, Long, Option[BigDecimal], Option[BigDecimal])) => Option[(Long, Long, Option[BigDecimal], Option[BigDecimal])] =
        s => Some(s._1, s._2, s._3, s._4)
      (steamId, userId, price, discountedPrice) <>(apply.tupled, unapply)
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

  def getUserById(id : Long) : Future[Option[User]] =
    db.run(userData.filter(_.id === id).result.headOption)

  def getUserByLogin(u : User) : Future[Option[User]] = {
    def getBySteamLogin(l : String) = db.run(userData.filter(_.steamLogin.like("%" + l + "%")).result.headOption)
    def getByGogLogin(l : String) = db.run(userData.filter(_.gogLogin.like("%" + l + "%")).result.headOption)
    u.steamLogin.map(getBySteamLogin).getOrElse(u.gogLogin.map(getByGogLogin).getOrElse(Future{None}))
  }

  def replaceGogData(user : Option[User], data : Seq[GogEntry]): Future[_] =
    user.map(u => {
      val ids = data.map(_.gogId).toSet
      val newOwnership = data.map(g => (g.gogId, u.id.get, g.price, g.discounted))
      val oldDataIdsQuery = gogData.filter(_.gogId.inSet(ids)).map(_.gogId)
      def insertNewData(oldDataIds : Seq[Long]) = {
        val newData = data.filter(d => !oldDataIds.contains(d.gogId)).map(d => (d.gogId, d.title))
        gogData ++= newData
      }
      lazy val  deleteOldOwnership = gogOwnershipData.filter(_.userId === u.id.get).delete
      lazy val  addNewOwnership = gogOwnershipData ++= newOwnership
      db.run((oldDataIdsQuery.result.flatMap(insertNewData) >> deleteOldOwnership >> addNewOwnership).transactionally)
    }).getOrElse(Future{true})

  def getGogEntries(user: Option[User], sources: Option[Boolean]): Future[Seq[GogEntry]] = {
    def condition(e: GogOwnershipData): Rep[Boolean] = {
      if (user.isEmpty) true
      else if (user.isDefined && sources.isEmpty) e.userId === user.get.id.get
      else e.price.isDefined === sources.get && e.userId === user.get.id.get
    }

    for {
      rows <- db.run(gogOwnershipData.filter(condition).join(gogData).on(_.gogId === _.gogId).result)
    } yield {
      rows.map(pair => GogEntry(pair._2._2, pair._2._1, pair._1._3, pair._1._4))
    }
  }

  def getSteamEntries(user: Option[User], sources: Option[Boolean]): Future[Seq[SteamEntry]] = {
    def condition(e: SteamOwnershipData): Rep[Boolean] = {
      if (user.isEmpty) true
      else if (user.isDefined && sources.isEmpty) e.userId === user.get.id.get
      else e.price.isDefined === sources.get && e.userId === user.get.id.get
    }

    for {
      rows <- db.run(steamOwnershipData.filter(condition).join(steamData).on(_.steamId === _.steamId).result)
    } yield {
      rows.map(pair => SteamEntry(pair._2._2, pair._2._1, pair._1._3, pair._1._4))
    }
  }

  def replaceSteamData(user : Option[User], data : Seq[SteamEntry]): Future[_] =
    user.map(u => {
      val ids = data.map(_.steamId).toSet
      val newOwnership = data.map(g => (g.steamId, u.id.get, g.price, g.discounted))
      val oldDataIdsQuery = steamData.filter(_.steamId.inSet(ids)).map(_.steamId)
      def insertNewData(oldDataIds : Seq[Long]) = {
        val newData = data.filter(d => !oldDataIds.contains(d.steamId)).map(d => (d.steamId, d.name))
        steamData ++= newData
      }
      lazy val  deleteOldOwnership = steamOwnershipData.filter(_.userId === u.id.get).delete
      lazy val  addNewOwnership = steamOwnershipData ++= newOwnership
      db.run((oldDataIdsQuery.result.flatMap(insertNewData) >> deleteOldOwnership >> addNewOwnership).transactionally)
    }).getOrElse(Future{true})

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
            steamOwnershipData.schema.create andThen
            (userData += User(None, Some("kongus"), Some("kongus99")))
      })
      Await.result(db.run(initialization), Duration.Inf)
    }
    start()
  }
  get
}
