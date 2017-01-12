name := "tigrs"

version := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.1"

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

  // also watch on locally published libraries
  watchSources <++= (managedClasspath in Compile) map { cp => cp.files }
)

lazy val root = project.in(file("."))
  .settings(
    publish := {},
    publishLocal := {}
  // run in Compile <<= (run in Compile in tigrsJVM)
  )
  .aggregate(frontend)

lazy val datatypes = crossProject.crossType(CrossType.Pure).in(file("datatypes"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= (
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided" ::
      "me.chrons" %%% "boopickle" % "1.2.5" ::
      "com.github.fdietze" %%% "pharg" % "0.1.0" ::
      "com.github.fdietze" %%% "vectory" % "0.1.0" ::
      Nil
    )
  )

lazy val datatypesJS = datatypes.js
lazy val datatypesJVM = datatypes.jvm

val circeVersion = "0.6.1"
lazy val modsParser = (project in file("modsparser"))
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
  .dependsOn(datatypesJVM)

val reactVersion = "15.4.2"
lazy val frontend = (project in file("frontend"))
  .settings(commonSettings: _*)
  .settings(
    persistLauncher in Test := false,

    libraryDependencies ++= (
      "org.scala-js" %%% "scalajs-dom" % "0.9.1" ::
      "com.github.japgolly.scalajs-react" %%% "core" % "0.11.3" ::
      "me.chrons" %%% "diode-react" % "1.1.0" ::
      "com.chuusai" %%% "shapeless" % "2.3.2" ::
      "com.github.fdietze" %%% "scalajs-react-custom-component" % "0.1.0" ::
      "com.github.fdietze" %%% "scala-js-d3v4-selection" % "1.0.3" ::
      "com.github.fdietze" %%% "scala-js-d3v4-collection" % "1.0.2" ::
      "com.github.fdietze" %%% "scala-js-d3v4-dispatch" % "1.0.2" ::
      "com.github.fdietze" %%% "scala-js-d3v4-quadtree" % "1.0.2" ::
      "com.github.fdietze" %%% "scala-js-d3v4-timer" % "1.0.3" ::
      "com.github.fdietze" %%% "scala-js-d3v4-force" % "1.0.4" ::
      "com.github.fdietze" %%% "scala-js-d3v4-zoom" % "1.1.1" ::
      "com.github.fdietze" %%% "scala-js-d3v4-transition" % "1.0.3" ::
      "com.github.fdietze" %%% "scala-js-d3v4-drag" % "1.0.2" ::
      Nil
    ),

    // React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
    jsDependencies ++= Seq(
      "org.webjars.bower" % "react" % reactVersion
        / "react-with-addons.js"
        minified "react-with-addons.min.js"
        commonJSName "react",

      "org.webjars.bower" % "react" % reactVersion
        / "react-dom.js"
        minified "react-dom.min.js"
        dependsOn "react-with-addons.js"
        commonJSName "ReactDOM",

      "org.webjars.bower" % "react" % reactVersion
        / "react-dom-server.js"
        minified "react-dom-server.min.js"
        dependsOn "react-dom.js"
        commonJSName "ReactDOMServer"
    )
  )
  .enablePlugins(ScalaJSPlugin, WorkbenchPlugin)
  .dependsOn(datatypesJS)
