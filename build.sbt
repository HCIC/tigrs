import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

name := "tigrs"

scalaVersion := "2.11.8"

scalaJSUseRhino in Global := false // execute js with node

libraryDependencies ++= (
  "org.scala-js" %%% "scalajs-dom" % "0.9.0" ::
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1" ::
  "com.github.japgolly.scalacss" %%% "ext-react" % "0.4.1" ::
  "me.chrons" %%% "diode-react" % "0.5.2" ::
  "org.singlespaced" %%% "scalajs-d3" % "0.3.3" ::
  Nil
)

// React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
jsDependencies ++= Seq(

  "org.webjars.bower" % "react" % "15.0.2"
    / "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",

  "org.webjars.bower" % "react" % "15.0.2"
    / "react-dom.js"
    minified "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM",

  "org.webjars.bower" % "react" % "15.0.2"
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
scalacOptions += "-Xplugin-require:scalaxy-streams"

scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))

scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"

autoCompilerPlugins := true

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")
