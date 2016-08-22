package tigrs

import scala.scalajs.js
import scala.scalajs.js.JSApp
import js.Dynamic.{global => g}
import js.DynamicImplicits._

object TutorialApp extends JSApp {
  def main(): Unit = {
    val elasticlunr = g.require("elasticlunr")
    println(elasticlunr.version)

    val fs = g.require("fs")
    fs.writeFile("/tmp/test", "Hey there!", { (err: js.Dynamic) =>
      if (err)
        println(err)
      else
        println("The file was saved!")
    })

    val p = Publications(Nil)
    println(p)
  }
}
