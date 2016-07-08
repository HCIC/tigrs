package tigrs

import collection.mutable

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom._
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js.Dynamic.global
import scala.annotation.meta.field

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import js.JSConverters._

object Main extends JSApp {
  def main() {
    val nodeLimit = 100

    val xhr = new dom.XMLHttpRequest()
    xhr.open("GET", "data/fak01a.xml")
    xhr.onload = { (e: dom.Event) =>
      if (xhr.status == 200) {
        val publications = ModsParser.xmlToPublications(xhr.responseXML, nodeLimit)
        AppCircuit.dispatch(SetGraph(publications.toGraph))
      }
    }
    xhr.send()

    ReactDOM.render(modelConnect(mainView(_)), document.getElementById("container"))
  }

  val modelConnect = AppCircuit.connect(m => m)
  val mainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P(proxy =>
      <.div(
        modelConnect(g => GraphView(g.value.graph, 400, 400))
      ))
    .build
}
