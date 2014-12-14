name := "rbvector-root"
organization := "fi.rbvector"
version := "1.0.0"
scalaVersion := "2.11.4"
lazy val vector = project
lazy val benchmark = project.dependsOn(vector)
