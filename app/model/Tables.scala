package model

import javax.inject._

import controllers.MatchEntry
import play.api.db.slick.DatabaseConfigProvider
import services.GameOn._
import services._
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile
import slick.jdbc.meta.MTable
import slick.lifted.ProvenShape

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

@Singleton
class Tables @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit exec: ExecutionContext) {

  case class OwnershipData(id : Long, userId : Long, owned : Boolean)

  val dbConfig: DatabaseConfig[JdbcProfile] = dbConfigProvider.get[JdbcProfile]

  import dbConfig.driver.api._

  trait IdAccessor {
    def accessId: Rep[Long]
  }

  trait PriceAccessor {
    def accessPrice: Rep[Option[BigDecimal]]
    def accessDiscounted: Rep[Option[BigDecimal]]
  }

  val db = dbConfig.db

  val matchData = TableQuery[MatchData]
  val userData = TableQuery[UserData]
  val gogData = TableQuery[GogData]
  val gogOwnershipData = TableQuery[GogOwnershipData]
  val steamData = TableQuery[SteamData]
  val steamOwnershipData = TableQuery[SteamOwnershipData]
  val priceData = TableQuery[PriceData]
  val gameQueryData = TableQuery[GameQueryData]

  class UserData(tag: Tag) extends Table[User](tag, "USER_DATA") {
    def id = column[Long]("USER_DATA_ID", O.PrimaryKey, O.AutoInc)

    def gogLogin = column[Option[String]]("USER_DATA_GOG_LOGIN")

    def steamLogin = column[Option[String]]("USER_DATA_STEAM_LOGIN")

    def steamAlternate = column[Boolean]("USER_DATA_STEAM_LOGIN_ALTERNATE")

    def gogLoginUnique = index("USER_DATA_GOG_LOGIN_UNIQUE", gogLogin, unique = true)

    def steamLoginUnique = index("USER_DATA_STEAM_LOGIN_UNIQUE", steamLogin, unique = true)

    override def * : ProvenShape[User] = (id.?, steamLogin, steamAlternate, gogLogin) <> ((User.apply _).tupled, User.unapply)
  }

  class GogData(tag: Tag) extends Table[GogEntry](tag, "GOG_DATA") with IdAccessor with PriceAccessor{
    def gogId = column[Long]("GOG_DATA_GOG_ID", O.PrimaryKey)

    def link = column[String]("GOG_DATA_LINK")

    def title = column[String]("GOG_DATA_TITLE")

    def price = column[Option[BigDecimal]]("GOG_DATA_PRICE", O.SqlType("DECIMAL(6,2)"))

    def discountedPrice = column[Option[BigDecimal]]("GOG_DATA_PRICE_DISCOUNTED", O.SqlType("DECIMAL(6,2)"))

    def * : ProvenShape[GogEntry] = {

      val apply: (Long, String, String, Option[BigDecimal], Option[BigDecimal]) => GogEntry = (gogId, link, title, price, discountedPrice) => GogEntry(title, link, gogId, price, discountedPrice, owned = false)

      val unapply: GogEntry => Option[(Long, String, String, Option[BigDecimal], Option[BigDecimal])] = g => Some(g.gogId, g.link, g.title, g.price, g.discounted)
      (gogId, link, title, price, discountedPrice) <>(apply.tupled, unapply)
    }

    override def accessId: Rep[Long] = gogId

    override def accessPrice: Rep[Option[BigDecimal]] = price

    override def accessDiscounted: Rep[Option[BigDecimal]] = discountedPrice
  }

  class GogOwnershipData(tag: Tag) extends Table[OwnershipData](tag, "GOG_OWNERSHIP_DATA") with IdAccessor{
    def gogId: Rep[Long] = column[Long]("GOG_OWNERSHIP_DATA_GOG_ID")

    def userId = column[Long]("GOG_OWNERSHIP_DATA_USER_ID")

    def owned = column[Boolean]("GOG_OWNERSHIP_DATA_OWNED")

    def comboUnique = primaryKey("GOG_OWNERSHIP_DATA_COMBO_UNIQUE", (gogId, userId))

    def gogFk = foreignKey("GOG_DATA_FK", gogId, gogData)(_.gogId, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def userFk = foreignKey("USER_DATA_FK", userId, userData)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def * : ProvenShape[OwnershipData] = (gogId, userId, owned) <> ((OwnershipData.apply _).tupled, OwnershipData.unapply)

    override def accessId : Rep[Long] = userId
  }

  class SteamData(tag: Tag) extends Table[SteamEntry](tag, "STEAM_DATA") with IdAccessor with PriceAccessor{
    def steamId = column[Long]("STEAM_DATA_STEAM_ID", O.PrimaryKey)

    def name = column[String]("STEAM_DATA_NAME")

    def price = column[Option[BigDecimal]]("STEAM_DATA_PRICE", O.SqlType("DECIMAL(6,2)"))

    def discountedPrice = column[Option[BigDecimal]]("STEAM_DATA_PRICE_DISCOUNTED", O.SqlType("DECIMAL(6,2)"))

    def * : ProvenShape[SteamEntry] = {

      val apply: (Long, String, Option[BigDecimal], Option[BigDecimal]) => SteamEntry = (steamId, name, price, discountedPrice) => SteamEntry(name, steamId, price, discountedPrice, owned = true)

      val unapply: (SteamEntry) => Option[(Long, String, Option[BigDecimal], Option[BigDecimal])] = s => Some((s.steamId, s.name, s.price, s.discounted))
      (steamId, name, price, discountedPrice) <>(apply.tupled, unapply)
    }

    override def accessId: Rep[Long] = steamId

    override def accessPrice: Rep[Option[BigDecimal]] = price

    override def accessDiscounted: Rep[Option[BigDecimal]] = discountedPrice
  }

  class SteamOwnershipData(tag: Tag) extends Table[OwnershipData](tag, "STEAM_OWNERSHIP_DATA") with IdAccessor {

    def steamId = column[Long]("STEAM_OWNERSHIP_DATA_STEAM_ID")

    def userId = column[Long]("STEAM_OWNERSHIP_DATA_USER_ID")

    def owned = column[Boolean]("STEAM_OWNERSHIP_DATA_OWNED")

    def comboUnique = primaryKey("STEAM_OWNERSHIP_DATA_COMBO_UNIQUE", (steamId, userId))

    def steamFk = foreignKey("STEAM_DATA_FK", steamId, steamData)(_.steamId, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def userFk = foreignKey("USER_DATA_FK", userId, userData)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def * : ProvenShape[OwnershipData] = (steamId, userId, owned) <> ((OwnershipData.apply _).tupled, OwnershipData.unapply)

    override def accessId: Rep[Long] = userId
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

  class PriceData(tag: Tag) extends Table[PriceEntry](tag, "PRICE_DATA") {

    def steamId = column[Long]("PRICE_DATA_STEAM_ID")

    def userId = column[Long]("PRICE_DATA_USER_ID")

    def name = column[String]("PRICE_DATA_NAME")

    def host = column[String]("PRICE_DATA_HOST")

    def link = column[String]("PRICE_DATA_LINK")

    def price = column[BigDecimal]("PRICE_DATA_PRICE", O.SqlType("DECIMAL(6,2)"))

    def steamFk = foreignKey("PRICE_DATA_STEAM_FK", steamId, steamData)(_.steamId, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade)

    def userFk = foreignKey("PRICE_DATA_USER_FK", userId, userData)(_.id, onUpdate = ForeignKeyAction.Restrict, onDelete = ForeignKeyAction.Cascade)

    def steamLinkUnique = primaryKey("PRICE_DATA_STEAM_USER_LINK_UNIQUE", (steamId, userId, link))

    override def * : ProvenShape[PriceEntry] = (steamId, userId, name, host, link, price) <> ((PriceEntry.apply _).tupled, PriceEntry.unapply)

  }

  case class GameQueryEntity(userId : Long, steamId : Long, query : String, site : String, selectedResult : Option[String]) {
    def this(userId: Long, steamId : Long, query : GameQuery) = this(userId, steamId, query.query, query.site, query.selectedResult)

    val gameQuery = GameQuery(query, site, Seq(), selectedResult)
  }

  class GameQueryData(tag: Tag) extends Table[GameQueryEntity](tag, "GAME_QUERY_DATA") {

    def userId = column[Long]("GAME_QUERY_DATA_USER_ID")

    def steamId = column[Long]("GAME_QUERY_DATA_STEAM_ID")

    def query = column[String]("GAME_QUERY_DATA_QUERY")

    def site = column[String]("GAME_QUERY_DATA_SITE")

    def selectedResult = column[Option[String]]("GAME_QUERY_DATA_RESULT")

    def steamFk = foreignKey("GAME_QUERY_DATA_STEAM_FK", steamId, steamData)(_.steamId, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def userFk = foreignKey("GAME_QUERY_DATA_USER_FK", userId, userData)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

    def steamLinkUnique = primaryKey("GAME_QUERY_DATA_SITE_UNIQUE", (userId, steamId, site))

    override def * : ProvenShape[GameQueryEntity] = (userId, steamId, query, site, selectedResult) <> ((GameQueryEntity.apply _).tupled, GameQueryEntity.unapply)

  }

  def getQueryData(userId : Long, steamId : Long) : Future[Seq[GameQuery]] = {
    val query = for {e <- gameQueryData if e.steamId === steamId && e.userId === userId} yield e
    for {result <- db.run(query.result)} yield result.map(_.gameQuery)
  }

  def getQueryData(site : String, steamIds : Set[Long]) : Future[Map[Long, Seq[(Long, GameQuery)]]] = {
    val query = for {e <- gameQueryData if e.site === site && e.steamId.inSet(steamIds)} yield e
    for {result <- db.run(query.result)} yield result.groupBy(_.steamId).mapValues(_.map(r => (r.userId, r.gameQuery)))
  }

  def changeQueryData(userId : Long, steamId : Long, insertData : GameQuery, queryOrSelected : Either[String, Option[String]]) : Future[Int] = {
    val query = for { e <- gameQueryData if e.steamId === steamId && e.userId === userId && e.site === insertData.site } yield e
    lazy val updateQuery = queryOrSelected.fold(l => query.map(d => (d.query, d.selectedResult)).update((l, None)), query.map(_.selectedResult).update(_))
    db.run(gameQueryData += new GameQueryEntity(userId, steamId, insertData)).recoverWith({ case _: Exception => db.run(updateQuery)})
  }

  def replacePrices(steamIds : Set[Long], prices: Seq[PriceEntry]): Future[_] = {
    val deleteOldPrices = priceData.filter(_.steamId.inSet(steamIds)).delete
    val addNewPrices = priceData ++= prices
    db.run((deleteOldPrices >> addNewPrices).transactionally)
  }

  def getPrices(steamEntries: Seq[SteamEntry]): Future[Seq[PriceEntry]] = {
    val steamIds = steamEntries.map(_.steamId)
    db.run(priceData.filter(_.steamId.inSet(steamIds)).result)
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

  def getAllUsers: Future[Seq[User]] =
    db.run(userData.result)

  def getByGogLogin(l : String): Future[Seq[User]] = db.run(userData.filter(_.gogLogin.like("%" + l + "%")).result)

  def getBySteamLogin(l : String): Future[Seq[User]] = db.run(userData.filter(_.steamLogin.like("%" + l + "%")).result)


  def getSteamUser(username: String) : Future[Option[User]] = db.run(userData.filter(_.steamLogin.like(username)).result.headOption)

  def getGogUser(username: String) : Future[Option[User]] = db.run(userData.filter(_.gogLogin.like(username)).result.headOption)

  def addUser(u: User) : Future[Option[User]] = {
    val userWithId  = (userData returning userData.map(_.id) into ((user,id) => user.copy(id=Some(id)))) += u
    db.run(userWithId.map(Some(_)))
  }

  def updateSteamAlternate(userId: Long, steamAlternate: Boolean) : Future[Option[User]]= {
    db.run(userData.filter(_.id === userId).map(_.steamAlternate).update(steamAlternate).andThen(userData.filter(_.id === userId).result.headOption))
  }


  def getGogEntries(user: Option[User], sources: Option[Boolean]): Future[Seq[GogEntry]] = {
    def condition(e: GogOwnershipData, g : GogData): Rep[Boolean] = {
      if (user.isEmpty) true
      else if (user.isDefined && sources.isEmpty) e.userId === user.get.id.get
      else e.owned === sources.get && e.userId === user.get.id.get
    }
    db.run(gogOwnershipData.join(gogData).on(_.gogId === _.gogId).filter((condition _).tupled).result).map(_.map(p => p._2.copy(owned = p._1.owned)))
  }

  def getSteamEntries(user: Option[User], sources: Option[Boolean]): Future[Seq[SteamEntry]] = {
    def condition(e: SteamOwnershipData, s : SteamData): Rep[Boolean] = {
      if (user.isEmpty) true
      else if (user.isDefined && sources.isEmpty) e.userId === user.get.id.get
      else e.owned === sources.get && e.userId === user.get.id.get
    }
    db.run(steamOwnershipData.join(steamData).on(_.steamId === _.steamId).filter((condition _).tupled).result).map(_.map(p => p._2.copy(owned = p._1.owned)))
  }

  def getSteamEntryById(steamId : Long): Future[SteamEntry] = db.run(steamData.filter(_.steamId === steamId).result.head)

  def replaceSteamData(user : User, data : Seq[SteamEntry]): Future[_] = replaceData(steamData, steamOwnershipData, data, user)

  def replaceGogData(user : User, data : Seq[GogEntry]): Future[_] = replaceData(gogData, gogOwnershipData, data, user)

  private def replaceData[Q <: ShopEntry, V <: Table[OwnershipData] with IdAccessor, T <: Table[Q] with IdAccessor with PriceAccessor]
    (oldData: TableQuery[T], oldOwnership: TableQuery[V], data : Seq[Q], u : User) = {

    val replaceOwnership =
      oldOwnership.filter(_.accessId === u.id.get).delete >> (oldOwnership ++= data.map(g => OwnershipData(g.id, u.id.get, g.owned)))

    val upsertPrices = {
      def updates(v: Q) = (for {e <- oldData if e.accessId === v.id} yield (e.accessPrice, e.accessDiscounted)).update((v.price, v.discounted))
      val ids = data.map(_.id).toSet
      val oldDataIdsQuery = oldData.filter(_.accessId.inSet(ids)).map(_.accessId).result
      oldDataIdsQuery.flatMap(oldDataIds => {
        val newData = data.filter(d => !oldDataIds.contains(d.id))
        val priceUpdates = DBIO.sequence(data.filter(_.price.isDefined).map(updates))
        (oldData ++= newData) >> priceUpdates
      })
    }
    db.run((upsertPrices >> replaceOwnership).transactionally)
  }


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
            priceData.schema.create andThen
            gameQueryData.schema.create andThen
            (userData += User(None, Some("kongus"), steamAlternate = false, Some("kongus99")))
      })
      Await.result(db.run(initialization), Duration.Inf)
    }
    start()
  }
  get
}
