package models

import config.ZapalyticsAppConfig
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, DbCommonsColumnMappers, SlickUtil}
import org.scalastr.core.{NostrEvent, NostrNoteId}
import play.api.libs.json.Json
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class ZapDb(
    id: Sha256Digest,
    author: SchnorrPublicKey,
    user: SchnorrPublicKey,
    invoice: LnInvoice,
    nodeId: NodeId,
    amount: MilliSatoshis,
    date: Long)

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

  class ZabTable(tag: Tag) extends Table[ZapDb](tag, schemaName, "zaps") {

    def id: Rep[Sha256Digest] = column("id", O.PrimaryKey)

    def author: Rep[SchnorrPublicKey] = column("author")

    def user: Rep[SchnorrPublicKey] = column("user")

    def invoice: Rep[LnInvoice] = column("invoice")

    def nodeId: Rep[NodeId] = column("node_id")

    def amount: Rep[MilliSatoshis] = column("amount")

    def date: Rep[Long] = column("date")

    def * : ProvenShape[ZapDb] =
      (id, author, user, invoice, nodeId, amount, date).<>(ZapDb.tupled,
                                                           ZapDb.unapply)
  }
}
