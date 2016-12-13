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

import Math._

case class GraphConfig(
  simConfig: SimulationConfig,
  hovered: Option[graph.Vertex] = None,
  highlightedVertices: Set[graph.Vertex] = Set.empty
)

object GraphViewCanvas extends D3[DirectedGraph[graph.Vertex]]("GraphViewCanvas") {
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
    val width = 900.0
    val height = 900.0

    val simulation = d3.forceSimulation()
      .force("center", d3.forceCenter(width / 2, height / 2))
      .force("charge", d3.forceManyBody())
      .force("link", d3.forceLink())

    simulation.on("tick", (e: Event) => {
      updateVisualization()
    })

    override def update(p: Props) = Callback {
      canvas.attr("width", width).attr("height", height)

      val vertexData = p.vertices.toJSArray
      val edgeData = p.edges.map(e => new D3E(e.in.asInstanceOf[D3V], e.out.asInstanceOf[D3V])).toJSArray

      simulation
        .nodes(vertexData.asInstanceOf[js.Array[D3V]])
      simulation.force("link")
        .links(edgeData)

      simulation.alpha(1).restart()
    }

    def updateVisualization() {
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
        context.fillStyle = d.asInstanceOf[graph.Vertex] match {
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

// object GraphView extends D3[DirectedGraph[graph.Vertex]]("GraphView") {
//   @js.native
//   trait D3V extends js.Object {
//     var x: js.UndefOr[Double] = js.native
//     var y: js.UndefOr[Double] = js.native
//   }

//   class D3E(
//     val source: D3V,
//     val target: D3V
//   ) extends d3js.Link[D3V]

//   class Backend($: Scope) extends D3Backend($) {
//     lazy val svg = component.append("svg")
//     lazy val vertices = svg.append("g")
//     lazy val edges = svg.append("g")
//     val force = d3.layout.force[D3V, D3E]().size((600.0, 600.0))

//     force.on("tick", (e: Event) => {
//       updateVisualization()
//     })

//     override def update(p: Props) = Callback {
//       svg.attr("width", 600).attr("height", 600)

//       // edges first => lazy val is evaluated first => g-element is appended before vertices
//       // => edges stay in background
//       val edgeData = p.edges.map(e => new D3E(e.in.asInstanceOf[D3V], e.out.asInstanceOf[D3V])).toJSArray
//       val edge = edges.selectAll("line").data(edgeData)

//       val vertexData = p.vertices.toJSArray
//       val vertex = vertices.selectAll("circle").data(vertexData)

//       vertex.enter()
//         .append("circle")

//       vertex
//         .attr("r", 5)
//         .attr("fill", { (d: graph.Vertex) =>
//           d match {
//             case _: graph.Publication => "#48D7FF"
//             case _: graph.PublicationSet => "#48D7FF"
//             case _: graph.Author => "#FF8A8E"
//             case _: graph.AuthorSet => "#FF8A8E"
//             case _: graph.Outlet => "#22E6AB"
//             case _: graph.Project => "#D720AF"
//             case _: graph.Keyword => "black"
//           }
//         })

//       vertex.exit()
//         .remove()

//       edge.enter()
//         .append("line")

//       edge
//         .attr("stroke", "#8F8F8F")

//       edge.exit()
//         .remove()

//       force.nodes(vertexData.asInstanceOf[js.Array[D3V]]).links(edgeData)
//       force.start()
//     }

//     def updateVisualization() {
//       val vertex = vertices.selectAll("circle")
//       val edge = edges.selectAll("line")

//       vertex
//         .attr("cx", (d: graph.Vertex) => d.asInstanceOf[D3V].x)
//         .attr("cy", (d: graph.Vertex) => d.asInstanceOf[D3V].y)

//       edge
//         .attr("x1", (d: D3E) => d.source.asInstanceOf[D3V].x)
//         .attr("y1", (d: D3E) => d.source.asInstanceOf[D3V].y)
//         .attr("x2", (d: D3E) => d.target.asInstanceOf[D3V].x)
//         .attr("y2", (d: D3E) => d.target.asInstanceOf[D3V].y)
//     }

//   }

//   val backendFactory = new Backend(_)
// }
