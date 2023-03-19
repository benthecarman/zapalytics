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

  startF.map { _ =>
    val times = 1672574400L.to(TimeUtil.currentEpochSecond).by(86400)

    FutureUtil
      .foldLeftAsync((), times) { (_, start) =>
        val end = start + 86400
        logger.info(s"Finding events for $start to $end")
        findEvents(start, end)
      }
      .map(_ => logger.info("Done finding all events"))
  }

  def notFound(route: String): Action[AnyContent] = {
    Action { implicit request: MessagesRequest[AnyContent] =>
      NotFound(views.html.notFound(route))
    }
  }

  def index: Action[AnyContent] = {
    Action { implicit request: MessagesRequest[AnyContent] =>
      Ok(views.html.index())
    }
  }
}
