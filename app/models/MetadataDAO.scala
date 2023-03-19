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

case class MetadataStats(
    wos: Int,
    alby: Int,
    zbd: Int,
    fountain: Int,
    lnTips: Int,
    stackerNews: Int,
    numUsers: Int
) {
  def otherCount = numUsers - wos - alby - zbd - fountain - lnTips - stackerNews

  def pieChartUrl: String =
    s"https://quickchart.io/chart?chart={type:'pie',data:{labels:['WoS','Alby','Zebedee','Fountain','Ln.tips','stacker.news','Others'],datasets:[{label:'Count',data:[$wos,$alby,$zbd,$fountain,$lnTips,$stackerNews,$otherCount]}]}}&backgroundColor=white&width=1000&height=600&format=png&version=2.9.3"
}

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
    MappedColumnType.base[LightningAddress, String](_.value,
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

  def calcMetadataStats(): Future[MetadataStats] = {
    val lnAddrsA = table.filter(_.lud16.isDefined).map(_.lud16.get).result

    val action = for {
      lnAddrs <- lnAddrsA
      lower = lnAddrs.map(_.value.toLowerCase)
    } yield {
      val wos = lower.count(_.value.contains("@walletofsatoshi.com"))
      val alby = lower.count(_.value.contains("@getalby.com"))
      val zbd = lower.count(_.value.contains("@zbd.gg"))
      val fountain = lower.count(_.value.contains("@fountain.fm"))
      val lnTips = lower.count(_.value.contains("@ln.tips"))
      val stackerNews = lower.count(_.value.contains("@stacker.news"))
      MetadataStats(wos = wos,
                    alby = alby,
                    zbd = zbd,
                    fountain = fountain,
                    lnTips = lnTips,
                    stackerNews = stackerNews,
                    numUsers = lnAddrs.size)
    }

    safeDatabase.run(action)
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
