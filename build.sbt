name := "cata"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.8"

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
  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.tpolecat" %% "atto-core"  % "0.6.5",
  "org.scalatest" %% "scalatest" % "3.0.6" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

mainClass in (Compile, run) := Some("net.kurnevsky.Main")
