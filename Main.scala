package tutorial.webapp

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExport

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

object TutorialApp extends JSApp {
  def main(): Unit = {

    val vdom = <.div(<.button(^.onClick --> Callback.alert("pressed"), "test"))

    ReactDOM.render(vdom, document.getElementById("container"))
  }
}
