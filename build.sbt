import sbt.Keys.resolvers

lazy val commonSettings = Seq(
  organization := "net.katsstuff",
  scalaVersion := "2.12.4",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-unused-import"
  )
)

lazy val common = project.settings(
  commonSettings,
  name := "scammander",
  version := "0.1",
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
)

lazy val sponge7 = project
  .settings(
    commonSettings,
    name := "scammander-sponge7",
    version := "0.1",
    resolvers += "Sponge" at "http://repo.spongepowered.org/maven",
    libraryDependencies += "org.spongepowered" % "spongeapi" % "7.0.0"
  )
  .dependsOn(common)

lazy val bukkit = project
  .settings(
    commonSettings,
    name := "scammander-bukkit",
    version := "0.1",
    resolvers += "Spigot" at "https://hub.spigotmc.org/nexus/content/repositories/snapshots",
    resolvers += "BungeeCord" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies += "org.spigotmc" % "spigot-api" % "1.12-R0.1-SNAPSHOT"
  )
  .dependsOn(common)
