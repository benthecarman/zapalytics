package config

import akka.actor.ActorSystem
import com.typesafe.config.Config
import grizzled.slf4j.Logging
import models._
import org.bitcoins.commons.config._
import org.bitcoins.db.{DbAppConfig, DbManagement, JdbcProfileComponent}

import java.nio.file.{Files, Path, Paths}
import scala.concurrent._
import scala.jdk.CollectionConverters._
import scala.util.Properties

/** Configuration for Zapalytics
  *
  * @param directory
  *   The data directory of the application
  * @param configOverrides
  *   Optional sequence of configuration overrides
  */
case class ZapalyticsAppConfig(
    private val directory: Path,
    override val configOverrides: Vector[Config])(implicit system: ActorSystem)
    extends DbAppConfig
    with JdbcProfileComponent[ZapalyticsAppConfig]
    with DbManagement
    with Logging {
  implicit val ec: ExecutionContextExecutor = system.dispatcher
  override val moduleName: String = ZapalyticsAppConfig.moduleName
  override type ConfigType = ZapalyticsAppConfig

  override val appConfig: ZapalyticsAppConfig = this

  import profile.api._

  override def newConfigOfType(configs: Vector[Config]): ZapalyticsAppConfig =
    ZapalyticsAppConfig(directory, configs)

  val baseDatadir: Path = directory

  lazy val nostrRelays: Vector[String] = {
    if (config.hasPath("nostr.relays")) {
      config.getStringList(s"nostr.relays").asScala.toVector
    } else Vector.empty
  }

  override def start(): Future[Unit] = {
    logger.info(s"Initializing setup")

    if (Files.notExists(baseDatadir)) {
      Files.createDirectories(baseDatadir)
    }

    val numMigrations = migrate().migrationsExecuted
    logger.info(s"Applied $numMigrations")

    Future.unit
  }

  override def stop(): Future[Unit] = Future.unit

  override lazy val dbPath: Path = baseDatadir

  override val allTables: List[TableQuery[Table[_]]] = {
    val zapTable: TableQuery[Table[_]] = ZapDAO()(ec, this).table
    val metadataTable: TableQuery[Table[_]] = MetadataDAO()(ec, this).table

    List(zapTable, metadataTable)
  }
}

object ZapalyticsAppConfig
    extends AppConfigFactoryBase[ZapalyticsAppConfig, ActorSystem] {

  val DEFAULT_DATADIR: Path = Paths.get(Properties.userHome, ".zapalytics")

  override def fromDefaultDatadir(confs: Vector[Config] = Vector.empty)(implicit
      ec: ActorSystem): ZapalyticsAppConfig = {
    fromDatadir(DEFAULT_DATADIR, confs)
  }

  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ActorSystem): ZapalyticsAppConfig =
    ZapalyticsAppConfig(datadir, confs)

  override val moduleName: String = "zapalytics"
}
