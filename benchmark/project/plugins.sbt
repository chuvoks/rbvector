// Comment to get more information during initialization
//logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// sbt-jmh plugin - pulls in JMH dependencies too
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.1.7")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.5.0")

