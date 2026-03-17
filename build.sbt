name := "akka-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.17"

val httpVersion = "10.2.10"

//lazy val akkaVersion = sys.props.getOrElse("akka.version", "2.10.17")
lazy val akkaVersion = sys.props.getOrElse("akka.version", "2.7.0")

// Run in a separate JVM, to make sure sbt waits until all threads have
// finished before returning.
// If you want to keep the application running while executing other
// sbt tasks, consider https://github.com/spray/sbt-revolver/
fork := true

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.8",

  // HTTP client for Downloader
  "com.typesafe.akka" %% "akka-http-core" % httpVersion,
  "com.typesafe.akka" %% "akka-http" % httpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % httpVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % httpVersion % Test,

  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.15" % Test,

  // HTML parsing
  "org.jsoup" % "jsoup" % "1.17.2",

  // SSL certificate generation
  "org.bouncycastle" % "bcpkix-jdk15to18" % "1.78")
