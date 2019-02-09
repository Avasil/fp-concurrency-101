import sbtcrossproject.{CrossType, crossProject}

val monixVersion = "3.0.0-RC2"
val http4sVersion = "0.20.0-M5"
val circeVersion = "0.11.1"

lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.12.8",
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
)

lazy val shared =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared"))
    .settings(commonSettings)
    .settings(
      name := "shared",
      libraryDependencies ++= Seq(
        "io.circe" %% "circe-core" % circeVersion,
        "io.circe" %% "circe-generic" % circeVersion,
        "io.circe" %% "circe-parser" % circeVersion,
        "io.circe" %% "circe-generic-extras" % circeVersion
      )
    )

lazy val sharedJS = shared.js
lazy val sharedJVM = shared.jvm

lazy val server = (project in file("server"))
  .settings(commonSettings)
  .settings(
    name := "tanks-server",
    libraryDependencies ++= Seq(
      "io.monix" %% "monix" % monixVersion,
      "org.http4s" %% "http4s-core" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,

    ),
    fork := true
  ).dependsOn(sharedJVM)

lazy val client = (project in file("client"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings)
  .settings(
    name := "tanks-client",
    scalaJSUseMainModuleInitializer := true,
    scalaJSUseMainModuleInitializer in Test := false,
    libraryDependencies ++= Seq(
      "io.monix" %%% "monix" % monixVersion,
      "org.scala-js" %%% "scalajs-dom" % "0.9.6",
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "io.circe" %%% "circe-generic-extras" % circeVersion
    )
  ).dependsOn(sharedJS)