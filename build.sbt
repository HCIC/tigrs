name := "tigrs"

version := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.0"

lazy val commonSettings = Seq(
  // scalaxy (faster collection operations)
  // scalacOptions += "-Xplugin-require:scalaxy-streams",
  // scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams")),
  // scalacOptions in Test += "-Xplugin-disable:scalaxy-streams",
  // autoCompilerPlugins := true,
  // addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4"),

  scalacOptions ++=
    "-encoding" :: "UTF-8" ::
    "-unchecked" ::
    "-deprecation" ::
    "-explaintypes" ::
    "-feature" ::
    "-language:_" ::
    "-Xlint:_" ::
    "-Ywarn-unused" ::
    // "-Xdisable-assertions" ::
    // "-optimize" ::
    // "-Yopt:_" :: // enables all 2.12 optimizations
    // "-Yinline" :: "-Yinline-warnings" ::
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
      "com.github.fdietze" %%% "pharg" % "0.1.0-SNAPSHOT" ::
      Nil
    )
  )

lazy val datatypesJS = datatypes.js
lazy val datatypesJVM = datatypes.jvm

lazy val modsParser = (project in file("modsparser"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= (
      "org.scala-lang.modules" %% "scala-xml" % "1.0.5" ::
      "com.lihaoyi" %% "upickle" % "0.4.1" ::
      Nil
    ),
    scalacOptions += "-optimize"
  )
  .dependsOn(datatypesJVM)

lazy val indexer = (project in file("indexer"))
  .settings(commonSettings: _*)
  .settings(
    // scalaJSUseRhino in Global := false,
    jsEnv := NodeJSEnv().value,
    jsDependencies ++= Seq(
      "org.webjars.npm" % "elasticlunr" % "0.9.5"
        / "release/elasticlunr.js"
        minified "release/elasticlunr.min.js"
        commonJSName "Elasticlunr"
    )
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(datatypesJS)

val reactVersion = "15.3.2"
lazy val frontend = (project in file("frontend"))
  .settings(commonSettings: _*)
  .settings(
    persistLauncher := true,
    persistLauncher in Test := false,

    libraryDependencies ++= (
      "org.scala-js" %%% "scalajs-dom" % "0.9.1" ::
      "com.github.japgolly.scalajs-react" %%% "core" % "0.11.3" ::
      "me.chrons" %%% "diode-react" % "1.1.0" ::
      "com.github.fdietze" %%% "scalajs-react-d3-force-layout" % "0.1.0-SNAPSHOT" ::
      "org.scala-lang.modules" %% "scala-async" % "0.9.6" ::
      "com.chuusai" %%% "shapeless" % "2.3.2" ::
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
        commonJSName "ReactDOMServer",

      "org.webjars.npm" % "elasticlunr" % "0.9.5"
        / "release/elasticlunr.js"
        minified "release/elasticlunr.min.js"
        commonJSName "Elasticlunr",

      "org.webjars.npm" % "dexie" % "1.4.1"
        / "dist/dexie.js"
        minified "dist/dexie.min.js"
        commonJSName "Dexie"
    )
  )
  .enablePlugins(ScalaJSPlugin, WorkbenchPlugin)
  .dependsOn(datatypesJS)
