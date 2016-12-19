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

import graph._
import collection.breakOut

import Math._

case class GraphProps(
  graph: DirectedGraphData[Vertex, VertexInfo, EdgeInfo],
  dimensions: Vec2,
  simConfig: SimulationConfig,
  visConfig: VisualizationConfig
)

object GraphViewCanvas extends D3[GraphProps]("GraphViewCanvas") {
  import js.Dynamic.global
  val d3 = global.d3

  class Backend($: Scope) extends D3Backend($) {
    lazy val canvas = d3.select(component).append("canvas")
    lazy val context = canvas.node().asInstanceOf[raw.HTMLCanvasElement].getContext("2d")

    var hoveredVertex: Option[VertexInfo] = None
    var highlightedVertices: Set[VertexInfo] = Set.empty
    var highlightedEdges: Set[EdgeInfo] = Set.empty
    var grayedVertices: Iterable[VertexInfo] = Nil
    var grayedEdges: Iterable[EdgeInfo] = Nil

    var normalVertices: Iterable[VertexInfo] = Nil
    var normalEdges: Iterable[EdgeInfo] = Nil

    val simulation = d3.forceSimulation()
      .force("center", d3.forceCenter())
      .force("gravityx", d3.forceX())
      .force("gravityy", d3.forceY())
      .force("repel", d3.forceManyBody())
      .force("link", d3.forceLink())

    simulation.on("tick", (e: Event) => {
      updateVisualization($.props.runNow())
    })

    def nodes = simulation.nodes().asInstanceOf[js.Array[VertexInfo]]
    def links = simulation.force("link").links().asInstanceOf[js.Array[EdgeInfo]]

    override def init(p: Props) = Callback {
      canvas.on("mousemove", () => mouseMove($.props.runNow()))
    }

    def mouseMove(p: Props) {
      val d3Vertex = simulation.find(d3.event.x, d3.event.y, 100).asInstanceOf[js.UndefOr[VertexInfo]].toOption
      d3Vertex match {
        case Some(v) =>
          highlight(p, v)
          AppCircuit.dispatch(HoverVertex(v.vertex))
        case None =>
          unHighlight(p)
          AppCircuit.dispatch(UnHoverVertex)
      }

      updateVisualization($.props.runNow())
    }

    def highlight(p: Props, v: VertexInfo) {
      import p.graph

      hoveredVertex = Some(v)
      highlightedVertices = graph.neighbours(v.vertex).map(graph.vertexData)
      highlightedEdges = graph.incidentEdges(v.vertex).map(graph.edgeData)
      grayedVertices = graph.vertexData.values.filterNot(v => highlightedVertices(v) || (hoveredVertex contains v))
      grayedEdges = graph.edgeData.values.filterNot(highlightedEdges)
      normalVertices = Nil
      normalEdges = Nil
    }

    def unHighlight(p: Props) {
      import p.graph

      hoveredVertex = None
      highlightedVertices = Set.empty
      highlightedEdges = Set.empty
      grayedVertices = Nil
      grayedEdges = Nil
      normalVertices = nodes
      normalEdges = links
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

        val vertexData = graph.vertices.map(graph.vertexData).toJSArray
        val edgeData = graph.edges.map(graph.edgeData).toJSArray

        simulation.nodes(vertexData)
        simulation.force("link").links(edgeData)

        if (hoveredVertex.isDefined)
          highlight(p, hoveredVertex.get)
        else
          unHighlight(p)

        simulation.alpha(1).restart()

      }

    }

    def updateVisualization(p: Props) {
      import p.dimensions._
      import p.visConfig._

      def r(v: VertexInfo) = radiusOffset + radiusFactor * pow(v.weight, radiusExponent)
      def w(e: EdgeInfo) = widthOffset + widthFactor * pow(e.weight, widthExponent)

      context.clearRect(0, 0, width, height)

      context.strokeStyle = "#8F8F8F"
      normalEdges.foreach { (e: EdgeInfo) =>
        context.beginPath()
        context.moveTo(e.source.x, e.source.y)
        context.lineTo(e.target.x, e.target.y)
        context.lineWidth = w(e)
        context.stroke()
      }

      normalVertices.foreach { (v: VertexInfo) =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, r(v), 0, 2 * Math.PI)
        context.fillStyle = v.vertex match {
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.AuthorSet => "#FF8A8E"
        }
        context.fill()
      }

      context.strokeStyle = "#DDDDDD"
      grayedEdges.foreach { e =>
        context.beginPath()
        context.moveTo(e.source.x, e.source.y)
        context.lineTo(e.target.x, e.target.y)
        context.lineWidth = w(e)
        context.stroke()
      }

      grayedVertices.foreach { (v: VertexInfo) =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, r(v), 0, 2 * Math.PI)
        context.fillStyle = v.vertex match {
          case _: graph.PublicationSet => "#E6F9FF"
          case _: graph.AuthorSet => "#FFE6E6"
        }
        context.fill()
      }

      context.strokeStyle = "black"
      highlightedEdges.foreach { e =>
        context.beginPath()
        context.moveTo(e.source.x, e.source.y)
        context.lineTo(e.target.x, e.target.y)
        context.lineWidth = w(e)
        context.stroke()
      }

      highlightedVertices.foreach { (v: VertexInfo) =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, r(v), 0, 2 * Math.PI)
        context.fillStyle = v.vertex match {
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.AuthorSet => "#FF8A8E"
        }
        context.fill()
      }

      hoveredVertex.foreach { v =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, r(v), 0, 2 * Math.PI)
        context.fillStyle = v.vertex match {
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.AuthorSet => "#FF8A8E"
        }
        context.fill()
        context.beginPath()
        context.arc(v.x, v.y, r(v) + 3, 0, 2 * Math.PI)
        context.strokeStyle = "black"
        context.lineWidth = 2
        context.stroke()
      }

    }

  }

  val backendFactory = new Backend(_)
}
