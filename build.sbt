name := "scammander"
organization := "net.katsstuff"
version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-unused-import"
)

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
