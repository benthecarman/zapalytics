package models

import config.ZapalyticsAppConfig
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, DbCommonsColumnMappers, SlickUtil}
import org.bitcoins.lnurl.LnURL
import org.scalastr.core.{NostrEvent, NostrNoteId}
import play.api.libs.json.Json
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class MetadataDb(
    user: SchnorrPublicKey,
    lud06: Option[LnURL],
    lud16: Option[LightningAddress],
    nodeId: Option[NodeId],
    date: Long)

case class MetadataDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: ZapalyticsAppConfig)
    extends CRUD[MetadataDb, SchnorrPublicKey]
    with SlickUtil[MetadataDb, SchnorrPublicKey] {

  import profile.api._

  private val mappers = new DbCommonsColumnMappers(profile)
  import mappers._

  implicit val lnurlMapper: BaseColumnType[LnURL] =
    MappedColumnType.base[LnURL, String](_.toString, LnURL.fromString)

  implicit val lnAddressMapper: BaseColumnType[LightningAddress] =
    MappedColumnType.base[LightningAddress, String](_.toString,
                                                    LightningAddress(_))

  override val table: TableQuery[MetadataTable] = TableQuery[MetadataTable]

  override def createAll(ts: Vector[MetadataDb]): Future[Vector[MetadataDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[SchnorrPublicKey]): Query[MetadataTable, MetadataDb, Seq] =
    table.filter(_.user.inSet(ids))

  override protected def findAll(
      ts: Vector[MetadataDb]): Query[MetadataTable, MetadataDb, Seq] =
    findByPrimaryKeys(ts.map(_.user))

  def safeCreateAction(t: MetadataDb): DBIO[MetadataDb] = {
    table.filter(_.user === t.user).result.headOption.flatMap {
      case Some(existing) =>
        if (t.date > existing.date) {
          table.filter(_.user === t.user).update(t).map(_ => t)
        } else DBIO.successful(existing)
      case None =>
        super.createAction(t)
    }
  }

  def safeCreate(t: MetadataDb): Future[MetadataDb] = {
    safeDatabase.run(safeCreateAction(t))
  }

  class MetadataTable(tag: Tag)
      extends Table[MetadataDb](tag, schemaName, "metadata") {

    def user: Rep[SchnorrPublicKey] = column("user", O.PrimaryKey)

    def lud06: Rep[Option[LnURL]] = column("lud06")

    def lud16: Rep[Option[LightningAddress]] = column("lud16")

    def nodeId: Rep[Option[NodeId]] = column("node_id")

    def date: Rep[Long] = column("date")

    def * : ProvenShape[MetadataDb] =
      (user, lud06, lud16, nodeId, date).<>(MetadataDb.tupled,
                                            MetadataDb.unapply)
  }
}
