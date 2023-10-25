package controllers

import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.crypto.SchnorrPublicKey

object Utils {

  def isCustodial(domain: String): Boolean = {
    domain match {
      case "lnbits.com"            => true
      case "walletofsatoshi.com"   => true
      case "getalby.com"           => true
      case "ln.tips"               => true
      case "stacker.news"          => true
      case "strike.army"           => true
      case "coincorner.io"         => true
      case "zbd.gg"                => true
      case "fountain.fm"           => true
      case "starbackr.me"          => true
      case "getcurrent.io"         => true
      case "current.ninja"         => true
      case "current.red"           => true
      case "current.tips"          => true
      case "current.fyi"           => true
      case "nostrplebs.com"        => true
      case "bitrefill.me"          => true
      case "geyser.fund"           => true
      case "vida.live"             => true
      case "vida.page"             => true
      case "pay.bitcoinjungle.app" => true
      case "bottlepay.me"          => true
      case "coinos.io"             => true
      case "noah.me"               => true
      case "satsback.com"          => true
      case "pay.bbw.sv"            => true
      case _                       => false
    }
  }

  // todo incomplete list
  def isCustodial(nodeId: NodeId): Boolean = {
    nodeId.hex match {
      case "022bd0aa893db4ac890e457cca8c83f112518d6941bf9153dab4bf904620503a78" => // lnbits.com
        true
      case "030a58b8653d32b99200a2334cfe913e51dc7d155aa0116c176657a4f1722677a3" => // alby
        true
      case "035e4ff418fc8b5554c5d9eea66396c227bd429a3251c8cbc711002ba215bfc226" => // wallet of satoshi
        true
      case "0251fff168b58b74e9b476af5a515b91fe0540a3681bc97fbb65379a807aea5f66" => // zebedee lnd0
        true
      case "03d6b14390cd178d670aa2d57c93d9519feaae7d1e34264d8bbb7932d47b75a50d" => // zebedee lnd1
        true
      case "02c3e01efd4f1944e9a50939f34cb275716fcd438769cbc0126b015677fa3b187e" => // zebedee lnd3
        true
      case "03bf2ff8699e5528f65d41656d405c4002dd2415e4491e945fd465890bc3a9ce23" => // zebedee lnd4
        true
      case "03d506016e3e0e540ac26d557a412520ea24990ca9405d410c24122f648752b830" => // zebedee lnd5
        true
      case "0349cb2f33d5542432b016405a22dfda18617d87abe4718e61c45909b8a5449329" => // zebedee lnd6
        true
      case "03ac0cf6da1916725f86d49ab35275b7b362054845e85c33ac181118aac266ebb7" => // zebedee lnd7
        true
      case "02c3b0963276dc5f031a9147c3df203d6a03e194aa2934a821fa7709adc926263a" => // zebedee lnd8
        true
      case _ => false
    }
  }

  def isCustodial(author: SchnorrPublicKey): Boolean = {
    author.hex match {
      case "be1d89794bf92de5dd64c1e60f6a2c70c140abac9932418fee30c5c637fe9479" => // wallet of satoshi
        true
      case "79f00d3f5a19ec806189fcab03c1be4ff81d18ee4f653c88fac41fe03570f432" => // alby
        true
      case "c7063ccd7e9adc0ddd4b77c6bfabffc8399b41e24de3a668a6ab62ede2c8aabd" => // current
        true
      case "6538925ebfb661f418d8c7d074bee2e8afd778701dd89070c2da936d571e55c3" => // fountain
        true
      case "aa55a479ad6934d0fd78f3dbd88515cd1ca0d7a110812e711380d59df7598935" => // bitcoin jungle
        true
      case "8fe53b37518e3dbe9bab26d912292001d8b882de9456b7b08b615f912dc8bf4a" => // bitcoin beach wallet
        true
      case "6a69b9a70c28857e14fd429efabea77cb65ab6dfee3ec79b32ab1c4e7c02a232" => // zebedee
        true
      case "fcd720c38d9ee337188f47aac845dcd8f590ccdb4a928b76dde18187b4c9d37d" => // ln.tips
        true
      case "00009483d5e84e8850e5430654e61802fd2838cdf0ffa8fe774b4e9a63f52428" => // stacker.news
        true
      case "82c72b6400144dae700293329ed1b307df8c7cae0ad6cde44c6334a570dd1fde" => // strike.army
        true
      case "e43f16ab84552a8680d3ade518803770fa16c9835da0a0f5b376cddef7f12786" => // anon sats
        true
      case "84de35e2584d2b144aae823c9ed0b0f3deda09648530b93d1a2a146d1dea9864" => // snort.social
        true
      case "42b3db1ca9f73ea861cca1f5a9f74dadf97b6ff539cdf722ccae16119907dfe6" => // vida
        true
      case "4bb1cc354560bf907f8714870f34c1df07c920768938a62c1ae40d57260134b2" => // bitcoin voucher bot
        true
      case "2a5e4937b58413c0d5ff428b1ff1366a231ada4a6bf3ed054b7ead046d567c80" => // kollider
        true
      case "f58fcf63564c90b6c4ef395e233f75eb78767e91d952c5e3dfefeb22a03ea980" => // satsback.com
        true
      case _ => false
    }
  }

  def getAuthorName(key: SchnorrPublicKey): String = {
    key.hex match {
      case "1743bcc6d80fc182ced1971853769ff9373a2707aad744b24e8577c6dae83fd0" =>
        "ZapMe.tips"
      case "abd32a8bc530142cc04a23f9c07239dbbc6664f4f7eeceb8092c0e3530f94e9d" =>
        "ZeusPay"
      case "0827e302f2e1addb2ab7f56a15bbbc63ad8c4dbea72a054dffeb1d6a20557daa" =>
        "Zapper@semisol.dev"
      case "738ea36ef74b2ac80bfb3887b40637c7dcdf74ea6eed73c718b7193313b90f9b" =>
        "tal@nostr.me"
      case "9630f464cca6a5147aa8a35f0bcdd3ce485324e732fd39e09233b1d848238f31" =>
        "jb55"
      case "be1d89794bf92de5dd64c1e60f6a2c70c140abac9932418fee30c5c637fe9479" =>
        "Wallet of Satoshi"
      case "79f00d3f5a19ec806189fcab03c1be4ff81d18ee4f653c88fac41fe03570f432" =>
        "Alby"
      case "c7063ccd7e9adc0ddd4b77c6bfabffc8399b41e24de3a668a6ab62ede2c8aabd" =>
        "Current"
      case "6538925ebfb661f418d8c7d074bee2e8afd778701dd89070c2da936d571e55c3" =>
        "Fountain"
      case "aa55a479ad6934d0fd78f3dbd88515cd1ca0d7a110812e711380d59df7598935" =>
        "Bitcoin Jungle"
      case "8fe53b37518e3dbe9bab26d912292001d8b882de9456b7b08b615f912dc8bf4a" =>
        "Bitcoin Beach Wallet"
      case "6a69b9a70c28857e14fd429efabea77cb65ab6dfee3ec79b32ab1c4e7c02a232" =>
        "Zebedee"
      case "fcd720c38d9ee337188f47aac845dcd8f590ccdb4a928b76dde18187b4c9d37d" =>
        "ln.tips"
      case "00009483d5e84e8850e5430654e61802fd2838cdf0ffa8fe774b4e9a63f52428" =>
        "stacker.news"
      case "82c72b6400144dae700293329ed1b307df8c7cae0ad6cde44c6334a570dd1fde" =>
        "strike.army"
      case "e43f16ab84552a8680d3ade518803770fa16c9835da0a0f5b376cddef7f12786" =>
        "anon sats"
      case "84de35e2584d2b144aae823c9ed0b0f3deda09648530b93d1a2a146d1dea9864" =>
        "Snort.social"
      case "42b3db1ca9f73ea861cca1f5a9f74dadf97b6ff539cdf722ccae16119907dfe6" =>
        "Vida"
      case "4bb1cc354560bf907f8714870f34c1df07c920768938a62c1ae40d57260134b2" =>
        "Bitcoin voucher bot"
      case "2a5e4937b58413c0d5ff428b1ff1366a231ada4a6bf3ed054b7ead046d567c80" =>
        "Kollider"
      case "f58fcf63564c90b6c4ef395e233f75eb78767e91d952c5e3dfefeb22a03ea980" =>
        "Satsback.com"
      case _ => key.hex
    }
  }
}
