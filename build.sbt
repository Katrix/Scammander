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
  ),
  //Fixes repository not specified error
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ =>
    false
  },
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/Katrix-/Scammander"),
      "scm:git:github.com/Katrix-/Scammander",
      Some("scm:git:github.com/Katrix-/Scammander")
    )
  ),
  homepage := Some(url("https://github.com/Katrix-/Scammander")),
  developers := List(Developer("Katrix", "Nikolai Frid", "katrix97@hotmail.com", url("http://katsstuff.net/"))),
  autoAPIMappings := true
)

lazy val noPublishSettings = Seq(publish := {}, publishLocal := {}, publishArtifact := false)

lazy val common = project.settings(
  commonSettings,
  publishSettings,
  name := "scammander",
  version := "0.1",
  libraryDependencies += "com.chuusai"   %% "shapeless" % "2.3.3",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test
)

lazy val sponge7 = project
  .settings(
    commonSettings,
    publishSettings,
    name := "scammander-sponge7",
    version := "0.1",
    resolvers += "Sponge" at "http://repo.spongepowered.org/maven",
    libraryDependencies += "org.spongepowered" % "spongeapi" % "7.0.0"
  )
  .dependsOn(common)

lazy val bukkit = project
  .settings(
    commonSettings,
    publishSettings,
    name := "scammander-bukkit",
    version := "0.1",
    resolvers += "Spigot" at "https://hub.spigotmc.org/nexus/content/repositories/snapshots",
    resolvers += "BungeeCord" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies += "org.spigotmc" % "spigot-api" % "1.12-R0.1-SNAPSHOT"
  )
  .dependsOn(common)

lazy val scammanderRoot = project
  .in(file("."))
  .aggregate(common, sponge7, bukkit)
  .settings(
    noPublishSettings,
    //Fixes repository not specified error
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  )
  .enablePlugins(ScalaUnidocPlugin)
