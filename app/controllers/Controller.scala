package controllers

import akka.actor.ActorSystem
import config.ZapalyticsAppConfig
import grizzled.slf4j.Logging
import models._
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
    findZaps()
    findMetadata()
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
