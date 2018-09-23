name := "TSA1"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "org.vegas-viz" %% "vegas" % "0.3.11",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0",
  "com.typesafe.akka" %% "akka-actor" % "2.5.16",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.16" % Test,
  "com.typesafe.akka" %% "akka-http" % "10.1.5",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.1.5" % Test,
  "com.typesafe.akka" %% "akka-stream" % "2.5.16",
  "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.16" % Test,
  "com.typesafe.akka" %% "akka-http-xml" % "10.1.5",
  "org.scalanlp" %% "breeze" % "0.13.2"
)
