name := "tigrs"

version := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

lazy val commonSettings = Seq(

  scalacOptions ++=
    "-encoding" :: "UTF-8" ::
    "-unchecked" ::
    "-deprecation" ::
    "-explaintypes" ::
    "-feature" ::
    "-language:_" ::
    "-Xlint:_" ::
    "-Ywarn-unused" ::
    Nil,

  // watch managed library dependencies (only works with scala 2.11 currently)
  watchSources ++= (managedClasspath in Compile).map(_.files).value
)

lazy val root = project.in(file("."))
  .aggregate(frontend)
  .settings(
    publish := {},
    publishLocal := {},
    addCommandAlias("dev", "~frontend/fastOptJS::webpack"),
    addCommandAlias("clean", "; frontend/clean; frontendAssets/clean")
  )

lazy val datatypes = crossProject.crossType(CrossType.Pure).in(file("datatypes"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= (
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided" ::
      "io.suzaku" %%% "boopickle" % "1.2.6" ::
      "com.github.fdietze" %%% "pharg" % "0.1.1" ::
      "com.github.fdietze" %%% "vectory" % "0.1.0" ::
      Nil
    )
  )

lazy val datatypesJS = datatypes.js
lazy val datatypesJVM = datatypes.jvm

val circeVersion = "0.6.1"
lazy val modsParser = (project in file("modsparser"))
  .dependsOn(datatypesJVM)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++=
      "org.scala-lang.modules" %% "scala-xml" % "1.0.5" ::
      "io.circe" %% "circe-core" % circeVersion ::
      "io.circe" %% "circe-generic" % circeVersion ::
      "io.circe" %% "circe-parser" % circeVersion ::
      Nil,
    scalacOptions ++=
      // "-opt:l:classpath" ::
      Nil
  )

lazy val frontendAssets = project
  .enablePlugins(SbtWeb, ScalaJSWeb, WebScalaJSBundlerPlugin)
  .settings(
    scalaJSProjects := Seq(frontend),
    pipelineStages in Assets := Seq(scalaJSPipeline)
  )

import org.scalajs.core.tools.io.{VirtualJSFile, FileVirtualJSFile}
val reactVersion = "15.4.2"
lazy val frontend = (project in file("frontend"))
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin, WorkbenchPlugin)
  .dependsOn(datatypesJS)
  .settings(commonSettings: _*)
  .settings(
    persistLauncher in Test := false,

    libraryDependencies ++= (
      "org.scala-js" %%% "scalajs-dom" % "0.9.1" ::
      "com.github.japgolly.scalajs-react" %%% "core" % "0.11.3" ::
      "io.suzaku" %%% "diode" % "1.1.1" ::
      "io.suzaku" %%% "diode-react" % "1.1.1" ::
      "com.chuusai" %%% "shapeless" % "2.3.2" ::
      "com.github.fdietze" %%% "scalajs-react-custom-component" % "0.1.0" ::
      "com.github.fdietze" %%% "scala-js-d3v4" % "0.1.0-SNAPSHOT" ::
      "io.circe" %%% "circe-core" % circeVersion ::
      "io.circe" %%% "circe-generic" % circeVersion ::
      "io.circe" %%% "circe-parser" % circeVersion ::
      Nil
    ),

    // enableReloadWorkflow := true,
    // emitSourceMaps := false,

    //TODO: scalajs-react bundler support: https://github.com/japgolly/scalajs-react/pull/320
    // until then we are exposing react to the global namespace:
    // (https://scalacenter.github.io/scalajs-bundler/cookbook.html#global-namespace)
    npmDependencies in Compile ++= Seq(
      "react" -> reactVersion,
      "react-dom" -> reactVersion
    ),
    // Add a dependency to the expose-loader (which will expose react to the global namespace)
    npmDevDependencies in Compile += "expose-loader" -> "0.7.1",
    // Use a custom config file to export the JS dependencies to the global namespace,
    // as expected by the scalajs-react facade
    webpackConfigFile := Some(baseDirectory.value / "webpack.config.js"),
    webpackConfigFile in fullOptJS := Some(baseDirectory.value / "prod.webpack.config.js"),
    watchSources += baseDirectory.value / "webpack.config.js",

    // Use the output of Scala.js as a “launcher”
    scalaJSLauncher in (Compile, fastOptJS) := {
      Attributed.blank[VirtualJSFile](FileVirtualJSFile((fastOptJS in Compile).value.data))
    },
    scalaJSLauncher in (Compile, fullOptJS) := {
      Attributed.blank[VirtualJSFile](FileVirtualJSFile((fullOptJS in Compile).value.data))
    }
  )
