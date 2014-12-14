name := "rbvector"
organization := "fi.rbvector"
version := "1.0.0"

scalaVersion := "2.11.4"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint", "-optimise")
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

crossScalaVersions := Seq("2.10.4", "2.11.4")

testOptions in Test += Tests.Argument("-oF")

seq(bintrayPublishSettings:_*)

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
