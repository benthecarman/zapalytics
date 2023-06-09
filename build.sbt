val bitcoinsV = "1.9.7-105-84803fdf-SNAPSHOT"
val scalastrV = "0.0.0-68-e0fc5343-SNAPSHOT"
val akkaV = "2.6.20"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers +=
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots"

lazy val root = project
  .in(file("."))
  .enablePlugins(PlayScala, DebianPlugin)
  .settings(
    name := "zapalytics",
    version := "0.1.0",
    scalaVersion := "2.13.10",
    maintainer := "benthecarman",
    libraryDependencies ++= Seq(
      guice,
      "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test,
      "org.bitcoin-s" %% "bitcoin-s-lnurl" % bitcoinsV withSources () withJavadoc (),
      "org.bitcoin-s" %% "bitcoin-s-db-commons" % bitcoinsV withSources () withJavadoc (),
      "org.scalastr" %% "client" % scalastrV withSources () withJavadoc (),
      "com.typesafe.akka" %% "akka-stream" % akkaV withSources () withJavadoc (),
      "com.typesafe.akka" %% "akka-actor-typed" % akkaV withSources () withJavadoc (),
      "com.typesafe.akka" %% "akka-serialization-jackson" % akkaV withSources () withJavadoc (),
      "com.typesafe.akka" %% "akka-slf4j" % akkaV withSources () withJavadoc (),
      "com.google.zxing" % "core" % "3.5.1" withSources () withJavadoc (),
      "com.danielasfregola" %% "twitter4s" % "8.0",
      "com.softwaremill.sttp.client3" %% "akka-http-backend" % "3.8.3",
      "com.bot4s" %% "telegram-akka" % "5.6.1",
      "ch.qos.logback" % "logback-classic" % "1.2.11"
    ),
    dependencyOverrides ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.11"
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-Xfatal-warnings",
      "-Xlint:adapted-args",
      "-Xlint:nullary-unit",
      "-Xlint:inaccessible",
      "-Xlint:infer-any",
      "-Xlint:missing-interpolator",
      "-Xlint:eta-sam",
      "-Xfatal-warnings",
      "-unchecked",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard",
      "-Ypatmat-exhaust-depth",
      "off"
    ),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
  )
