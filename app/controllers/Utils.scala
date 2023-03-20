package controllers

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
      case "pay.bbw.sv"            => true
      case _                       => false
    }
  }
}
