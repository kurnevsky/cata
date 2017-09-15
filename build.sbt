name := "cata"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.3"

organization := "net.kurnevsky"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-encoding", "utf8",
  "-Xfuture",
  "-Xlint",
  "-Yno-generic-signatures" // https://issues.scala-lang.org/browse/SI-7449
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.typesafeRepo("releases")
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.tpolecat" %% "atto-core"  % "0.6.1-M1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

mainClass in (Compile, run) := Some("net.kurnevsky.Main")
