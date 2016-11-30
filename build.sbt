name := """Aggregator"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

routesImport += "binders.CustomBinders._"

libraryDependencies ++= Seq(
  cache,
  ws,
  "com.typesafe.play" %% "play-slick" % "2.0.0",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.0.0",
  "com.h2database" % "h2" % "1.4.193",
  "org.postgresql" % "postgresql" % "9.4.1211",
  "org.jsoup" % "jsoup" % "1.10.1",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)

fork in run := false