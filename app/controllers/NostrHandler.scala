package controllers

import grizzled.slf4j.Logging
import models._
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.lnurl.LnURL
import org.scalastr.client.NostrClient
import org.scalastr.core._
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Try

trait NostrHandler extends Logging { self: Controller =>

  def findEvents(startTime: Long, endTime: Long): Future[Unit] = {
    var lastEvent = TimeUtil.currentEpochSecond

    val clients = config.nostrRelays.map { relay =>
      new NostrClient(relay, None) {

        override def unsubOnEOSE: Boolean = true

        override def processEvent(
            subscriptionId: String,
            event: NostrEvent): Future[Unit] = {
          lastEvent = TimeUtil.currentEpochSecond
          if (event.kind == NostrKind.Zap) {
            // todo may need to do batching
            Try {
              val user = event.tags
                .find(_.value.head.asOpt[String].contains("p"))
                .get
                .value(1)
                .as[String]
              val invoiceStr = event.tags
                .find(_.value.head.asOpt[String].contains("bolt11"))
                .get
                .value(1)
                .as[String]
              val invoice = LnInvoice.fromString(invoiceStr)
              ZapDb(
                id = event.id,
                author = event.pubkey,
                user = SchnorrPublicKey(user),
                invoice = invoice,
                nodeId = invoice.nodeId,
                amount =
                  invoice.amount.map(_.toMSat).getOrElse(MilliSatoshis.zero),
                date = event.created_at
              )
            }.toOption
              .map(zapDAO
                .create(_)
                .map(db => logger.info(s"Saved zap: ${db.id.hex}"))
                .recover(_ => ()))
              .getOrElse(Future.unit)
          } else if (event.kind == NostrKind.Metadata) {
            // todo may need to do batching
            Try {
              val metadata = Json.parse(event.content).as[Metadata]
              if (metadata.lud06.isDefined || metadata.lud16.isDefined) {
                Some(
                  MetadataDb(
                    user = event.pubkey,
                    lud06 = metadata.lud06.flatMap(LnURL.fromStringOpt),
                    lud16 = metadata.lud16.flatMap(s =>
                      Try(LightningAddress(s)).toOption),
                    nodeId = None,
                    date = event.created_at
                  ))
              } else None
            }.toOption.flatten
              .map(metadataDAO
                .safeCreate(_)
                .map(db => logger.info(s"Saved metadata: ${db.user.hex}"))
                .recover(_ => ()))
              .getOrElse(Future.unit)
          } else Future.unit
        }

        override def processNotice(notice: String): Future[Unit] =
          Future.unit
      }
    }

    val filter = NostrFilter(
      ids = None,
      authors = None,
      kinds = Some(Vector(NostrKind.Zap, NostrKind.Metadata)),
      `#e` = None,
      `#p` = None,
      since = Some(startTime),
      until = Some(endTime),
      limit = None
    )

    val fs = clients.map { client =>
      for {
        _ <- client.start()
        _ <- client.subscribe(filter)
        _ = logger.info(s"Subscribed to ${client.url}")
        _ <- AsyncUtil.awaitCondition(
          () => lastEvent < TimeUtil.currentEpochSecond - 3,
          interval = 1.second,
          maxTries = 500)
        _ <- Future.fromTry(Try(client.stop())).flatten.recover(_ => ())
      } yield ()
    }

    Future.sequence(fs).map(_ => logger.info("Done finding events"))
  }
}
