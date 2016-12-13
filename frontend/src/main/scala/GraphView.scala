package tigrs

import pharg._
import vectory._

import scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation._
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import fdietze.scalajs.react.components._

import tigrs.graph.Vertex

import Math._

case class GraphConfig(
  graph: DirectedGraph[Vertex],
  dimensions: Vec2
// simConfig: SimulationConfig,
// hovered: Option[Vertex] = None,
// highlightedVertices: Set[Vertex] = Set.empty
)

object GraphViewCanvas extends D3[GraphConfig]("GraphViewCanvas") {
  import js.Dynamic.global
  val d3 = global.d3

  @js.native
  trait D3V extends js.Object {
    var x: js.UndefOr[Double] = js.native
    var y: js.UndefOr[Double] = js.native
  }

  @ScalaJSDefined
  class D3E(
    val source: D3V,
    val target: D3V
  ) extends js.Object

  class Backend($: Scope) extends D3Backend($) {
    lazy val canvas = d3.select(component).append("canvas")
    lazy val context = canvas.node().asInstanceOf[raw.HTMLCanvasElement].getContext("2d")

    val simulation = d3.forceSimulation()
      .force("charge", d3.forceManyBody())
      .force("link", d3.forceLink())

    simulation.on("tick", (e: Event) => {
      updateVisualization($.props.runNow())
    })

    override def update(p: Props) = Callback {
      import p._
      import dimensions._

      canvas.attr("width", width).attr("height", height)

      val vertexData = graph.vertices.toJSArray
      val edgeData = graph.edges.map(e => new D3E(e.in.asInstanceOf[D3V], e.out.asInstanceOf[D3V])).toJSArray

      simulation
        .force("center", d3.forceCenter(width / 2, height / 2))
      simulation
        .nodes(vertexData.asInstanceOf[js.Array[D3V]])
      simulation.force("link")
        .links(edgeData)

      simulation.alpha(1).restart()
    }

    def updateVisualization(p: Props) {
      import p.dimensions._
      context.clearRect(0, 0, width, height)

      // draw links
      context.strokeStyle = "#8F8F8F"
      context.beginPath()
      simulation.force("link").links().asInstanceOf[js.Array[D3E]].foreach { (d: D3E) =>
        context.moveTo(d.source.x, d.source.y)
        context.lineTo(d.target.x, d.target.y)
      }
      context.stroke()

      // draw nodes
      simulation.nodes().asInstanceOf[js.Array[D3V]].foreach { (d: D3V) =>
        context.moveTo(d.x, d.y)
        context.beginPath()
        context.arc(d.x, d.y, 4.5, 0, 2 * Math.PI)
        context.fillStyle = d.asInstanceOf[Vertex] match {
          case _: graph.Publication => "#48D7FF"
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.Author => "#FF8A8E"
          case _: graph.AuthorSet => "#FF8A8E"
          case _: graph.Outlet => "#22E6AB"
          case _: graph.Project => "#D720AF"
          case _: graph.Keyword => "black"
        }
        context.fill()
      }
    }

  }

  val backendFactory = new Backend(_)
}
