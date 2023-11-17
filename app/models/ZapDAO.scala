package models

import config.ZapalyticsAppConfig
import controllers.Utils
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, DbCommonsColumnMappers, SlickUtil}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode

case class ZapDb(
    id: Sha256Digest,
    author: SchnorrPublicKey,
    user: SchnorrPublicKey,
    sender: Option[SchnorrPublicKey],
    invoice: LnInvoice,
    nodeId: NodeId,
    amount: MilliSatoshis,
    date: Long)

case class ZapStats(
    total: MilliSatoshis,
    count: Int,
    uniqueNodeIds: Int,
    uniqueReceivers: Int,
    uniqueSenders: Int,
    uniqueAuthors: Int,
    zapsByAuthor: Seq[(SchnorrPublicKey, MilliSatoshis)]
) {

  def custodialZaps: MilliSatoshis = {
    MilliSatoshis(zapsByAuthor.map { case (author, amt) =>
      if (Utils.isCustodial(author)) amt.toLong else 0
    }.sum)
  }

  def percentCustodial: BigDecimal =
    BigDecimal((custodialZaps.toLong.toDouble / total.toLong) * 100)
      .setScale(2, RoundingMode.HALF_DOWN)

  def averageZapAmount: MilliSatoshis = MilliSatoshis(total.toLong / count)
}

case class ZapDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: ZapalyticsAppConfig)
    extends CRUD[ZapDb, Sha256Digest]
    with SlickUtil[ZapDb, Sha256Digest] {

  import profile.api._

  private val mappers = new DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[ZabTable] = TableQuery[ZabTable]

  override def createAll(ts: Vector[ZapDb]): Future[Vector[ZapDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256Digest]): Query[ZabTable, ZapDb, Seq] =
    table.filter(_.id.inSet(ids))

  override protected def findAll(
      ts: Vector[ZapDb]): Query[ZabTable, ZapDb, Seq] =
    findByPrimaryKeys(ts.map(_.id))

  def getMissingUserKeys(): Future[Vector[SchnorrPublicKey]] = {
    val query = sql"""
      SELECT DISTINCT z.user
      FROM zaps z
      LEFT JOIN metadata m
      ON z.user = m.user
      WHERE m.user IS NULL
    """.as[String]

    safeDatabase.run(query).map(_.map(SchnorrPublicKey.fromHex))
  }

  def getMissingSenderKeys(): Future[Vector[SchnorrPublicKey]] = {
    val query = sql"""
      SELECT DISTINCT z.sender
      FROM zaps z
      LEFT JOIN metadata m
      ON z.sender = m.user
      WHERE z.sender IS NOT NULL
      AND m.user IS NULL
    """.as[String]

    safeDatabase.run(query).map(_.map(SchnorrPublicKey.fromHex))
  }

  def getZapsWithNoSender(): Future[Vector[Sha256Digest]] = {
    val query = table.filter(_.sender.isEmpty).map(_.id).result
    safeDatabase.runVec(query)
  }

  def calcZapStats(): Future[ZapStats] = {
    val maxZap = MilliSatoshis(Bitcoins(0.005))
    val fakers = Seq(
      SchnorrPublicKey(
        "0827e302f2e1addb2ab7f56a15bbbc63ad8c4dbea72a054dffeb1d6a20557daa"),
      SchnorrPublicKey(
        "738ea36ef74b2ac80bfb3887b40637c7dcdf74ea6eed73c718b7193313b90f9b"),
      SchnorrPublicKey(
        "3b1b67e5f7b815fcb07f5939fc04b050f18331407b2388fa1fcef11a2bed15a0"
      )
    )

    val valid = table
      .filter(_.amount < maxZap)
      .filterNot(_.author.inSet(fakers))

    val total = valid
      .map(_.amount)
      .sum
      .getOrElse(MilliSatoshis.zero)
      .result
    val count = valid.length.result
    val uniqueNodeIds = valid.map(_.nodeId).distinct.length.result
    val uniqueReceivers = valid.map(_.user).distinct.length.result
    val uniqueSenders = valid.map(_.sender).distinct.length.result
    val uniqueAuthors = valid.map(_.author).distinct.length.result

    val zapsByAuthorA = valid
      .groupBy(_.author)
      .map { case (author, zaps) =>
        (author, zaps.map(_.amount).sum.getOrElse(MilliSatoshis.zero))
      }
      .result

    val action = for {
      total <- total
      count <- count
      uniqueNodeIds <- uniqueNodeIds
      uniqueReceivers <- uniqueReceivers
      uniqueSenders <- uniqueSenders
      uniqueAuthors <- uniqueAuthors
      zapsByAuthor <- zapsByAuthorA
      zapsByAuthorSorted = zapsByAuthor
//        .filter(_._2 > MilliSatoshis(Satoshis(10_000_000)))
        .filter(_._2.toSatoshis.toLong % 1_000_000 != 0)
        .sortBy(_._2)(Ordering[MilliSatoshis])
        .reverse
    } yield ZapStats(total,
                     count,
                     uniqueNodeIds,
                     uniqueReceivers,
                     uniqueSenders,
                     uniqueAuthors,
                     zapsByAuthorSorted)

    safeDatabase.run(action)
  }

  class ZabTable(tag: Tag) extends Table[ZapDb](tag, schemaName, "zaps") {

    def id: Rep[Sha256Digest] = column("id", O.PrimaryKey)

    def author: Rep[SchnorrPublicKey] = column("author")

    def user: Rep[SchnorrPublicKey] = column("user")

    def sender: Rep[Option[SchnorrPublicKey]] = column("sender")

    def invoice: Rep[LnInvoice] = column("invoice")

    def nodeId: Rep[NodeId] = column("node_id")

    def amount: Rep[MilliSatoshis] = column("amount")

    def date: Rep[Long] = column("date")

    def * : ProvenShape[ZapDb] =
      (id, author, user, sender, invoice, nodeId, amount, date).<>(
        ZapDb.tupled,
        ZapDb.unapply)
  }
}
