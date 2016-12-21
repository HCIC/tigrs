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

  val hoverDistance = 30
  val collisionGap = 2
  val hoverBorderWidth = 5
  val maxTextWidth = 300

  class Backend($: Scope) extends D3Backend($) {
    lazy val canvas = d3.select(component).append("canvas")
    lazy val context = canvas.node().asInstanceOf[raw.HTMLCanvasElement].getContext("2d")
    lazy val labels = d3.select(component).append("div")
    var labelsData: js.Dynamic = js.undefined.asInstanceOf[js.Dynamic]

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
      .force("collision", d3.forceCollide())

    simulation.on("tick", (e: Event) => {
      draw($.props.runNow())
    })

    def nodes = simulation.nodes().asInstanceOf[js.Array[VertexInfo]]
    def links = simulation.force("link").links().asInstanceOf[js.Array[EdgeInfo]]

    override def init(p: Props) = Callback {
      // init lazy vals
      canvas
      context
      labels

      canvas.on("mousemove", () => mouseMove($.props.runNow()))
      canvas.on("mouseout", () => mouseOut($.props.runNow()))
      canvas.call(d3.zoom().on("zoom", () => zoomed($.props.runNow())))
    }

    def zoomed(p: Props) {
      import p.dimensions._

      d3.zoom().transform(canvas, d3.event.transform)
      draw(p)
    }

    def mouseMove(p: Props) {
      val t = d3.zoomTransform(canvas.node())
      val d3Vertex = simulation.find(t.invertX(d3.event.x), t.invertY(d3.event.y), hoverDistance).asInstanceOf[js.UndefOr[VertexInfo]].toOption
      d3Vertex match {
        case Some(v) =>
          highlight(p, v)
          AppCircuit.dispatch(HoverVertex(v.vertex))
        case None =>
          unHighlight(p)
          AppCircuit.dispatch(UnHoverVertex)
      }

      draw($.props.runNow())
    }

    def mouseOut(p: Props) {
      unHighlight(p)
      AppCircuit.dispatch(UnHoverVertex)
      draw($.props.runNow())
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

      def newOrChanged(get: Props => Any) = oldProps.isEmpty || get(p) != get(oldProps.get)

      if (newOrChanged(_.simConfig)) {
        simulation.force("gravityx").strength(simConfig.gravity)
        simulation.force("gravityy").strength(simConfig.gravity)
        simulation.force("repel").strength(-simConfig.repel)
        simulation.force("link").distance((e: EdgeInfo) => simConfig.linkDistance / (1 + e.weight))
        simulation.alpha(1).restart()
      }

      if (newOrChanged(_.visConfig.radiusOffset) || newOrChanged(_.visConfig.radiusFactor) || newOrChanged(_.visConfig.radiusExponent)) {
        simulation.force("collision").radius((v: VertexInfo) => vertexRadius(v, p.visConfig) + collisionGap)
        simulation.alpha(0.1).restart()
      }

      if (newOrChanged(_.dimensions)) {
        canvas.attr("width", width).attr("height", height)

        simulation.force("center").x(width / 2).y(height / 2)
        simulation.force("gravityx").x(width / 2)
        simulation.force("gravityy").y(height / 2)

        simulation.alpha(0.01).restart()
      }

      if (newOrChanged(_.visConfig.authorLabels) || newOrChanged(_.graph)) {
        val authors: Seq[VertexInfo] = graph.vertices.collect { case as: AuthorSet => graph.vertexData(as) }(breakOut)
        val displayedAuthors = authors.sortBy(-_.weight).take((visConfig.authorLabels * authors.size).toInt).toJSArray

        labelsData = labels.selectAll("div").data(displayedAuthors)

        labelsData.enter()
          .append("div")
          .text((v: VertexInfo) => v.vertex.asInstanceOf[AuthorSet].as.head.name.split(",")(0))
          .style("position", "absolute")
          .style("pointer-events", "none") // pass mouse events to canvas
          .style("width", s"${maxTextWidth}px")
          .style("text-align", "center")
          .style("text-shadow", "-1px -1px 0 white,  1px -1px 0 white, -1px 1px 0 white, 1px 1px 0 white")

        labelsData.exit()
          .remove()
      }

      if (newOrChanged(_.visConfig)) {
        draw(p)
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

    def vertexRadius(v: VertexInfo, c: VisualizationConfig) = c.radiusOffset + c.radiusFactor * pow(v.weight, c.radiusExponent)
    def edgeWidth(e: EdgeInfo, c: VisualizationConfig) = c.widthOffset + c.widthFactor * pow(e.weight, c.widthExponent)

    def draw(p: Props) {
      import p.dimensions._
      import p.visConfig

      context.save()
      context.clearRect(0, 0, width, height)

      val transform = d3.zoomTransform(canvas.node())
      context.translate(transform.x, transform.y)
      context.scale(transform.k, transform.k)

      context.strokeStyle = "#8F8F8F"
      normalEdges.foreach { (e: EdgeInfo) =>
        context.beginPath()
        context.moveTo(e.source.x, e.source.y)
        context.lineTo(e.target.x, e.target.y)
        context.lineWidth = edgeWidth(e, visConfig)
        context.stroke()
      }

      normalVertices.foreach { (v: VertexInfo) =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig), 0, 2 * Math.PI)
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
        context.lineWidth = edgeWidth(e, visConfig)
        context.stroke()
      }

      grayedVertices.foreach { (v: VertexInfo) =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig), 0, 2 * Math.PI)
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
        context.lineWidth = edgeWidth(e, visConfig)
        context.stroke()
      }

      highlightedVertices.foreach { (v: VertexInfo) =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig), 0, 2 * Math.PI)
        context.fillStyle = v.vertex match {
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.AuthorSet => "#FF8A8E"
        }
        context.fill()
      }

      hoveredVertex.foreach { v =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig), 0, 2 * Math.PI)
        context.fillStyle = v.vertex match {
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.AuthorSet => "#FF8A8E"
        }
        context.fill()
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig) + hoverBorderWidth / 2.0, 0, 2 * Math.PI)
        context.strokeStyle = "rgba(200,200,200, 0.7)"
        context.lineWidth = hoverBorderWidth
        context.stroke()
      }

      context.restore()

      labels.selectAll("div")
        .style("left", (v: VertexInfo) => s"${transform.applyX(v.x).asInstanceOf[Double] - (maxTextWidth / 2)}px")
        .style("top", (v: VertexInfo) => s"${transform.applyY(v.y).asInstanceOf[Double] - 10}px")
    }

  }

  val backendFactory = new Backend(_)
}
