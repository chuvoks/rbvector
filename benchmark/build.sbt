import JmhKeys._

name := "rbvector-benchmark"
organization := "fi.rbvector"
version := "1.0.0"

scalaVersion := "2.11.4"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint", "-optimise")
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

jmhSettings
version in Jmh := "1.3.3"
outputTarget in Jmh := target.value / s"scala-${scalaBinaryVersion.value}"

libraryDependencies ++= Seq(
 "fi.rbvector" %% "rbvector" % "1.0.0"
)
