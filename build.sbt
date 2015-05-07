name := "CSL"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Wabisabi ES Client Repo" at "https://raw.github.com/gphat/mvn-repo/master/releases/"

//resolvers += "Typesafe Public Repo" at "http://repo.typesafe.com/typesafe/releases"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.specs2" %% "specs2-core" % "2.4.16" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "2.4.16" % "test",
  "wabisabi" %% "wabisabi" % "2.1.0",
  "org.slf4j" % "slf4j-simple" % "1.6.4",
  "org.json4s" %% "json4s-jackson" % "3.2.10",
  //"com.typesafe.slick" %% "slick" % "3.0.0",
  //"com.typesafe.play" %% "anorm" % "2.4.0",
  "mysql" % "mysql-connector-java" % "5.1.12",
  "com.typesafe.play" %% "anorm" % "2.4.0-RC1"
)
