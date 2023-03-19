package controllers

import akka.actor.ActorSystem
import config.ZapalyticsAppConfig
import grizzled.slf4j.Logging
import models._
import org.bitcoins.core.util.{FutureUtil, TimeUtil}
import play.api.mvc._

import javax.inject.Inject
import scala.concurrent._

class Controller @Inject() (cc: MessagesControllerComponents)
    extends MessagesAbstractController(cc)
    with NostrHandler
    with Logging {

  implicit lazy val system: ActorSystem = {
    val system = ActorSystem("zapalytics")
    system.log.info("Akka logger started")
    system
  }
  implicit lazy val ec: ExecutionContext = system.dispatcher

  implicit lazy val config: ZapalyticsAppConfig =
    ZapalyticsAppConfig.fromDefaultDatadir()

  private val startF: Future[Unit] = config.start()

  val zapDAO: ZapDAO = ZapDAO()
  val metadataDAO: MetadataDAO = MetadataDAO()

  def notFound(route: String): Action[AnyContent] = {
    Action { implicit request: MessagesRequest[AnyContent] =>
      NotFound(views.html.notFound(route))
    }
  }

  def index: Action[AnyContent] = {
    Action.async { implicit request: MessagesRequest[AnyContent] =>
      val zapStatsF = zapDAO.calcZapStats()
      val metadataStatsF = metadataDAO.calcMetadataStats()

      for {
        zapStats <- zapStatsF
        metadataStats <- metadataStatsF
      } yield Ok(views.html.index(zapStats, metadataStats))
    }
  }

  def reindex(): Action[AnyContent] = {
    Action.async { implicit request: MessagesRequest[AnyContent] =>
      startF.flatMap { _ =>
        val interval = 86400
        val times = 1672574400L.to(TimeUtil.currentEpochSecond).by(interval)

        FutureUtil
          .foldLeftAsync((), times) { (_, start) =>
            val end = start + interval
            logger.info(s"Finding events for $start to $end")
            findEvents(start, end)
          }
          .map(_ => Ok("Done!"))
      }
    }
  }
}
