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

import scalacss.Defaults._
import scalacss.ScalaCssReact._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import org.singlespaced.d3js
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.Link

import js.JSConverters._

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

case class Vertex(r: Double) {
  override def toString = s"V(${r.toInt})"
}

case class RootModel(graph: Graph[Vertex, DiEdge])
case class AddVertex(v: Vertex)
case class AddEdge(e: DiEdge[Vertex])
case class RemoveVertex(v: Vertex)

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = {
    val vertexCount = 5
    val edgeCount = 3
    def rd = scala.util.Random.nextDouble
    val vertices = Array.tabulate(vertexCount)(i => Vertex(5 + rd * 10))
    def randomVertices(n: Int) = scala.util.Random.shuffle(vertices.toSeq).take(n)
    val edges = Array.tabulate(edgeCount)(i => {
      val vs = randomVertices(2);
      DiEdge(vs(0), vs(1))
    })
    RootModel(Graph.from(vertices, edges))
  }

  val graphHandler = new ActionHandler(zoomRW(_.graph)((m, v) => m.copy(graph = v))) {
    override def handle = {
      case AddVertex(v) => updated(value + v)
      case AddEdge(e) => updated(value + e)
      case RemoveVertex(v) => updated(value - v)
    }
  }
  val actionHandler = composeHandlers(graphHandler)
}

object Main extends JSApp {

  def main() {
    val sc = AppCircuit.connect(m => m)(mainView(_))
    ReactDOM.render(sc, document.getElementById("container"))
  }

  val mainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P(proxy =>
      <.div(
        <.button(^.onClick --> proxy.dispatch(AddVertex(Vertex(r = scala.util.Random.nextDouble * 20 + 10))), "add vertex"),
        <.button(^.onClick --> {
          val vs = scala.util.Random.shuffle(proxy.value.graph.nodes.toSeq).sortBy(_.degree).take(2)
          proxy.dispatch(AddEdge(DiEdge(vs(0).value, vs(1).value)))
        }, "add edge"),
        proxy.wrap(_.graph)(TigrsView(_, 400, 400)),
        proxy.wrap(_.graph)(ClassicView(_, 200, 200))
      ))
    .build
}

object TigrsView extends graphView.GraphView[Vertex, DiEdge] {
  override def charge(v: Vertex) = v.r * v.r * (-2)
  override def linkDistance(e: DiEdge[Vertex]) = 100
  override def styleVertices(sel: VertexSelection) = {
    super.styleVertices(sel)
      .attr("r", (d: D3Vertex) => d.v.r)
      .on("click", (d: D3Vertex) => AppCircuit.dispatch(RemoveVertex(d.v)))
  }
}

object ClassicView extends graphView.GraphView[Vertex, DiEdge]
