package controllers

import akka.actor.ActorSystem
import config.ZapalyticsAppConfig
import grizzled.slf4j.Logging
import models._
import org.bitcoins.core.util.{FutureUtil, TimeUtil}
import play.api.mvc._

import java.time.Instant
import java.time.temporal.ChronoUnit
import javax.inject.Inject
import scala.concurrent._
import scala.concurrent.duration._

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

  private def doReindex(): Future[Unit] = {
    logger.info("Starting reindex")
    val missingF = for {
      _ <- getZapUsers().recover { case x: Throwable =>
        logger.error("Failed to get zap users", x)
      }
      _ <- getZapSenders().recover { case x: Throwable =>
        logger.error("Failed to get zap senders", x)
      }
      _ <- getZapsWithMissingSenders().recover { case x: Throwable =>
        logger.error("Failed to get zaps with missing senders", x)
      }
    } yield ()

    missingF.flatMap { _ =>
      val interval = 86400
      val times = 1672574400L.to(TimeUtil.currentEpochSecond).by(interval)

      FutureUtil
        .foldLeftAsync((), times) { (_, start) =>
          val end = start + interval
          logger.info(s"Finding events for $start to $end")
          findEvents(start, end)
        }
        .map(_ => logger.info("Finished reindex"))
    }
  }

  def reindex(key: String): Action[AnyContent] = {
    Action.async { implicit request: MessagesRequest[AnyContent] =>
      if (key != config.adminKey) {
        Future.successful(Forbidden("Invalid admin key"))
      } else
        startF.flatMap { _ =>
          val _ = doReindex()
          Future.successful(Ok("Reindexing started"))
        }
    }
  }

  private def calcNextReindexTime(): Instant = {
    val now = Instant.now()
    val eightUTCToday = now
      .truncatedTo(ChronoUnit.DAYS)
      .plus(8, ChronoUnit.HOURS)
    if (now.isAfter(eightUTCToday)) {
      // need to get tomorrows settlement
      eightUTCToday.plus(1, ChronoUnit.DAYS)
    } else {
      eightUTCToday
    }
  }

  private def durationUntilNextReindex(): FiniteDuration = {
    val time = calcNextReindexTime()
    val now = Instant.now().toEpochMilli
    time
      .minusMillis(now)
      .toEpochMilli
      .milliseconds
  }

  // every day reindex the data
  startF.map { _ =>
    val initDelay = durationUntilNextReindex()
    val interval = 1.day
    system.scheduler.scheduleAtFixedRate(initDelay, interval) { () =>
      val _ = doReindex()
    }
  }
}
