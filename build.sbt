import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

name := "tigrs"

scalaVersion := "2.11.8"

scalaJSUseRhino in Global := false // execute js with node

scalaJSOptimizerOptions in (Compile, fullOptJS) ~= { _.withUseClosureCompiler(false) }

libraryDependencies ++= (
  "org.scala-js" %%% "scalajs-dom" % "0.9.1" ::
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1" ::
  "me.chrons" %%% "diode-react" % "1.0.0" ::
  "com.github.fdietze" %%% "scalajs-react-d3-force-layout" % "0.1-SNAPSHOT" ::
  Nil
)

// React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
jsDependencies ++= Seq(

  "org.webjars.bower" % "react" % "15.1.0"
    / "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "react",

  "org.webjars.bower" % "react" % "15.1.0"
    / "react-dom.js"
    minified "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM",

  "org.webjars.bower" % "react" % "15.1.0"
    / "react-dom-server.js"
    minified "react-dom-server.min.js"
    dependsOn "react-dom.js"
    commonJSName "ReactDOMServer"
)

// workbench
workbenchSettings

bootSnippet := "tigrs.Main().main();"

updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)

// scalaxy (faster collection operations)
// scalacOptions += "-Xplugin-require:scalaxy-streams"

// scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))

// scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"

// autoCompilerPlugins := true

// addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")

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
  Nil
