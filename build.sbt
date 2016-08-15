import com.lihaoyi.workbench.Plugin._

name := "tigrs"

scalaVersion in ThisBuild := "2.11.8"

lazy val root = project.in(file(".")).
  aggregate(tigrsJS, tigrsJVM).
  settings(
    publish := {},
    publishLocal := {},
    run in Compile <<= (run in Compile in tigrsJVM)
  )

lazy val tigrs = crossProject.in(file(".")).
  settings(
    name := "tigrs",
    version := "0.1-SNAPSHOT",

    // scalaxy (faster collection operations)
    scalacOptions += "-Xplugin-require:scalaxy-streams",

    scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams")),

    scalacOptions in Test += "-Xplugin-disable:scalaxy-streams",

    autoCompilerPlugins := true,

    addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4"),

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

    libraryDependencies ++= (
      "me.chrons" %%% "boopickle" % "1.2.4" ::
      "org.scala-graph" %%% "graph-core" % "1.11.1" ::
      Nil
    )
  )
  .jvmSettings(
    libraryDependencies ++= (
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided" ::
      "org.scala-lang.modules" %% "scala-xml" % "1.0.5" ::
      Nil
    )
  )
  .jsSettings(workbenchSettings ++ Seq(
    persistLauncher := true,
    persistLauncher in Test := false,

    scalaJSOptimizerOptions in (Compile, fullOptJS) ~= { _.withUseClosureCompiler(false) },

    libraryDependencies ++= (
      "org.scala-js" %%% "scalajs-dom" % "0.9.1" ::
      "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1" ::
      "me.chrons" %%% "diode-react" % "1.0.0" ::
      "com.github.fdietze" %%% "scalajs-react-d3-force-layout" % "0.1-SNAPSHOT" ::
      Nil
    ),

    // React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
    jsDependencies ++= Seq(

      "org.webjars.bower" % "react" % "15.3.0"
        / "react-with-addons.js"
        minified "react-with-addons.min.js"
        commonJSName "react",

      "org.webjars.bower" % "react" % "15.3.0"
        / "react-dom.js"
        minified "react-dom.min.js"
        dependsOn "react-with-addons.js"
        commonJSName "ReactDOM",

      "org.webjars.bower" % "react" % "15.3.0"
        / "react-dom-server.js"
        minified "react-dom-server.min.js"
        dependsOn "react-dom.js"
        commonJSName "ReactDOMServer",

      "org.webjars.npm" % "elasticlunr" % "0.9.0"
        / "release/elasticlunr.js"
        minified "release/elasticlunr.min.js"
        commonJSName "Elasticlunr",

      "org.webjars.npm" % "dexie" % "1.4.1"
        / "dist/dexie.js"
        minified "dist/dexie.min.js"
        commonJSName "Dexie"
    ),

    // also watch on locally published libraries
    watchSources <++=
      (managedClasspath in Compile) map { cp => cp.files },

    // workbench (refresh browser on compile)

    bootSnippet := "tigrs.Main().main();",

    // updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)

    refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)

  ): _*)

lazy val tigrsJVM = tigrs.jvm
lazy val tigrsJS = tigrs.js
