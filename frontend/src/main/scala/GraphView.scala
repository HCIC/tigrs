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

case class GraphProps(
  graph: DirectedGraph[Vertex],
  dimensions: Vec2,
  simConfig: SimulationConfig,
  visConfig: VisualizationConfig
)

object GraphViewCanvas extends D3[GraphProps]("GraphViewCanvas") {
  import js.Dynamic.global
  val d3 = global.d3

  @js.native
  trait D3V extends js.Object {
    var x: js.UndefOr[Double] = js.native
    var y: js.UndefOr[Double] = js.native
    var hovered: js.UndefOr[Boolean] = js.native
    var highlighted: js.UndefOr[Boolean] = js.undefined
  }

  @ScalaJSDefined
  class D3E(
    val source: D3V,
    val target: D3V,
    var highlighted: js.UndefOr[Boolean] = js.undefined
  ) extends js.Object

  class Backend($: Scope) extends D3Backend($) {
    lazy val canvas = d3.select(component).append("canvas")
    lazy val context = canvas.node().asInstanceOf[raw.HTMLCanvasElement].getContext("2d")
    var hovered = false

    val simulation = d3.forceSimulation()
      .force("center", d3.forceCenter())
      .force("gravityx", d3.forceX())
      .force("gravityy", d3.forceY())
      .force("repel", d3.forceManyBody())
      .force("link", d3.forceLink())

    simulation.on("tick", (e: Event) => {
      updateVisualization($.props.runNow())
    })

    override def init(p: Props) = Callback {
      canvas.on("mousemove", { () =>
        val p = $.props.runNow()
        val graph = p.graph

        simulation.nodes().asInstanceOf[js.Array[D3V]].foreach { (v: D3V) =>
          v.hovered = false
          v.highlighted = false
        }
        simulation.force("link").links().asInstanceOf[js.Array[D3E]].foreach { (e: D3E) =>
          e.highlighted = false
        }

        val d3Vertex = simulation.find(d3.event.x, d3.event.y, 100).asInstanceOf[js.UndefOr[D3V]].toOption
        d3Vertex match {
          case Some(v) =>
            v.hovered = true
            graph.neighbours(v.asInstanceOf[Vertex]).foreach { n =>
              n.asInstanceOf[D3V].highlighted = true
            }
            graph.incidentEdges(v.asInstanceOf[Vertex]).foreach { e =>
              e.asInstanceOf[js.Dynamic].d3e.highlighted = true
            }

            AppCircuit.dispatch(HoverVertex(v.asInstanceOf[Vertex]))
            hovered = true
          case None =>
            AppCircuit.dispatch(UnHoverVertex)
            hovered = false
        }

        updateVisualization($.props.runNow())
      })
    }

    override def update(p: Props, oldProps: Option[Props] = None) = Callback {
      import p._
      import dimensions._

      def newOrChanged(get: Props => AnyRef) = oldProps.isEmpty || get(p) != get(oldProps.get)

      if (newOrChanged(_.simConfig)) {
        simulation.force("gravityx").strength(simConfig.gravity)
        simulation.force("gravityy").strength(simConfig.gravity)
        simulation.force("repel").strength(-simConfig.repel)
        simulation.force("link").distance(simConfig.linkDistance)
        simulation.alpha(1).restart()
      }

      if (newOrChanged(_.visConfig)) {
        updateVisualization(p)
      }

      if (newOrChanged(_.dimensions)) {
        canvas.attr("width", width).attr("height", height)

        simulation.force("center").x(width / 2).y(height / 2)
        simulation.force("gravityx").x(width / 2)
        simulation.force("gravityy").y(height / 2)

        simulation.alpha(0.01).restart()
      }

      if (newOrChanged(_.graph)) {

        val vertexData = graph.vertices.toJSArray
        val edgeData = graph.edges.map { e =>
          val d3e = new D3E(e.in.asInstanceOf[D3V], e.out.asInstanceOf[D3V])
          e.asInstanceOf[js.Dynamic].d3e = d3e
          d3e
        }.toJSArray

        simulation
          .nodes(vertexData.asInstanceOf[js.Array[D3V]])
        simulation.force("link")
          .links(edgeData)

        simulation.alpha(1).restart()
      }

    }

    def updateVisualization(p: Props) {
      import p.dimensions._
      import p.visConfig.radius

      context.clearRect(0, 0, width, height)

      // draw nodes
      if (hovered) {
        context.lineWidth = 1
        val (highlightedEdges, edges) = simulation.force("link").links().asInstanceOf[js.Array[D3E]].partition { (e: D3E) =>
          e.highlighted.getOrElse(false)
        }

        var hoveredVertex: D3V = null
        val (highlightedVertices, vertices) = simulation.nodes().asInstanceOf[js.Array[D3V]].partition { (v: D3V) =>
          if (v.hovered.getOrElse(false)) hoveredVertex = v
          v.highlighted.getOrElse(false)
        }

        edges.foreach { e =>
          context.beginPath()
          context.moveTo(e.source.x, e.source.y)
          context.lineTo(e.target.x, e.target.y)
          context.strokeStyle = "#DDDDDD"
          context.stroke()
        }

        vertices.foreach { (v: D3V) =>
          context.moveTo(v.x, v.y)
          context.beginPath()
          context.arc(v.x, v.y, radius, 0, 2 * Math.PI)
          context.fillStyle = v.asInstanceOf[Vertex] match {
            case _: graph.PublicationSet => "#E6F9FF"
            case _: graph.AuthorSet => "#FFE6E6"
          }

          // context.globalAlpha = if (hovered) 1.0 else 0.5
          context.fill()

        }

        highlightedEdges.foreach { e =>
          context.beginPath()
          context.moveTo(e.source.x, e.source.y)
          context.lineTo(e.target.x, e.target.y)
          context.strokeStyle = "black"
          context.stroke()
        }

        highlightedVertices.foreach { (v: D3V) =>
          context.moveTo(v.x, v.y)
          context.beginPath()
          context.arc(v.x, v.y, radius, 0, 2 * Math.PI)
          context.fillStyle = v.asInstanceOf[Vertex] match {
            case _: graph.PublicationSet => "#48D7FF"
            case _: graph.AuthorSet => "#FF8A8E"
          }

          // context.globalAlpha = if (hovered) 1.0 else 0.5
          context.fill()
        }

        context.moveTo(hoveredVertex.x, hoveredVertex.y)
        context.beginPath()
        context.arc(hoveredVertex.x, hoveredVertex.y, radius, 0, 2 * Math.PI)
        context.fillStyle = hoveredVertex.asInstanceOf[Vertex] match {
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.AuthorSet => "#FF8A8E"
        }

        // context.globalAlpha = if (hovered) 1.0 else 0.5
        context.fill()

        context.beginPath()
        context.arc(hoveredVertex.x, hoveredVertex.y, radius + 3, 0, 2 * Math.PI)
        context.strokeStyle = "black"
        context.lineWidth = 2
        context.stroke()

      } else { // not hovered
        // draw links
        context.strokeStyle = "#8F8F8F"
        context.lineWidth = 1
        context.beginPath()
        simulation.force("link").links().asInstanceOf[js.Array[D3E]].foreach { (d: D3E) =>
          context.moveTo(d.source.x, d.source.y)
          context.lineTo(d.target.x, d.target.y)
        }
        context.stroke()

        simulation.nodes().asInstanceOf[js.Array[D3V]].foreach { (d: D3V) =>
          context.moveTo(d.x, d.y)
          context.beginPath()
          context.arc(d.x, d.y, radius, 0, 2 * Math.PI)
          context.fillStyle = d.asInstanceOf[Vertex] match {
            case _: graph.PublicationSet => "#48D7FF"
            case _: graph.AuthorSet => "#FF8A8E"
          }
          context.fill()
        }
      }
    }

  }

  val backendFactory = new Backend(_)
}
