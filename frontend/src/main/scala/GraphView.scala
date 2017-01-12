package tigrs

import pharg._
import vectory._

import scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation._
import org.scalajs.dom
import org.scalajs.dom._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import fdietze.scalajs.react.component._

import graph._
import collection.mutable
import collection.breakOut

import org.scalajs.d3v4._
import org.scalajs.d3v4.force._
import org.scalajs.d3v4.zoom._
import org.scalajs.d3v4.selection._

import Math._

case class GraphProps(
  graph: DirectedGraphData[Vertex, VertexInfo, EdgeInfo],
  dimensions: Vec2,
  simConfig: SimulationConfig,
  visConfig: VisualizationConfig
)

object GraphViewCanvas extends CustomComponent[GraphProps]("GraphViewCanvas") {
  import js.Dynamic.global
  val d3js = global.d3

  val hoverDistance = 30
  val collisionGap = 2
  val hoverBorderWidth = 5
  val hoverBorderColor = "rgba(200,200,200, 0.7)"
  val filterMatchBorderWidth = 10
  val filterMatchBorderColor = "rgba(136,255,130, 0.7)"
  val maxTextWidth = 300

  class Backend($: Scope) extends CustomBackend($) {
    lazy val canvas = d3js.select(component).append("canvas")
    lazy val context = canvas.node().asInstanceOf[raw.HTMLCanvasElement].getContext("2d")
    lazy val labels = d3js.select(component).append("div")
    var labelsData: js.Dynamic = js.undefined.asInstanceOf[js.Dynamic]

    var hoveredVertex: Option[VertexInfo] = None
    var selectedVertices: Iterable[VertexInfo] = Nil
    var filterMatchedVertices: Iterable[VertexInfo] = Nil
    var drawBgVertices: Iterable[VertexInfo] = Nil
    var drawBgEdges: Iterable[EdgeInfo] = Nil
    var drawFgVertices: Iterable[VertexInfo] = Nil
    var drawFgEdges: Iterable[EdgeInfo] = Nil

    val simulation = d3.forceSimulation[VertexInfo]()
      .force("center", d3.forceCenter())
      .force("gravityx", d3.forceX())
      .force("gravityy", d3.forceY())
      .force("repel", d3.forceManyBody())
      .force("link", d3.forceLink())
      .force("collision", d3.forceCollide())

    simulation.on("tick", () => draw())

    var transform: Transform = d3.zoomIdentity // stores current pan and zoom

    override def init() {
      // init lazy vals
      canvas
      context
      labels

      canvas.on("mousemove", () => mouseMove($.props.runNow()))
      canvas.on("mouseout", () => mouseOut($.props.runNow()))
      canvas.call(d3js.zoom().on("zoom", zoomed _))
    }

    def zoomed() {
      transform = d3.event.transform
      draw()
    }

    def mouseMove(p: Props) {
      val d3Vertex = simulation.find(transform.invertX(d3.event.x), transform.invertY(d3.event.y), hoverDistance).asInstanceOf[js.UndefOr[VertexInfo]].toOption
      d3Vertex match {
        case Some(v) =>
          hoveredVertex = Some(v)
          v match {
            case VertexInfo(AuthorSet(_, authors), _) => console.log(s"hover:\n ${authors.mkString("\n ")}")
            case VertexInfo(PublicationSet(_, publications), _) => console.log(s"hover:\n ${publications.mkString("\n ")}")
          }

          AppCircuit.dispatch(HoverVertex(v.vertex))
        case None =>
          hoveredVertex = None
          AppCircuit.dispatch(UnHoverVertex)
      }

      updateHighlight(p)
      draw()
    }

    def mouseOut(p: Props) {
      hoveredVertex = None
      AppCircuit.dispatch(UnHoverVertex)
      updateHighlight(p)
      draw()
    }

    var filtering: Boolean = false
    def updateFilter(p: Props) {
      import p.graph

      val filter = p.visConfig.filter.trim.toLowerCase
      filtering = filter.nonEmpty
      val matched = mutable.ArrayBuffer.empty[VertexInfo]
      if (filtering)
        for (v <- graph.vertexData.values) {
          import v._
          isMatchedByFilter = vertex.isInstanceOf[AuthorSet] && vertex.asInstanceOf[AuthorSet].as.exists(_.name.toLowerCase containsSlice filter)
          if (isMatchedByFilter) {
            matched += v
          }
        }
      filterMatchedVertices = matched

      for (v <- graph.vertexData.values) {
        import v._
        isMatchedNeighbour = vertex.isInstanceOf[PublicationSet] && graph.neighbours(vertex).exists(v => graph.vertexData(v).isMatchedByFilter)
      }
    }

    def updateHighlight(p: Props) {
      import p.graph
      val hovering = hoveredVertex.isDefined
      val hoveredNeighbours = hoveredVertex.map(v => graph.neighbours(v.vertex)).getOrElse(Set.empty)
      val hoveredIncidentEdges = hoveredVertex.map(v => graph.incidentEdges(v.vertex)).getOrElse(Set.empty)

      for (v <- graph.vertexData.values) {
        import v._
        isHoveredVertex = hovering && (hoveredVertex.get == v)
        isHoveredNeighbour = hovering && (hoveredNeighbours contains vertex)

        foreground = (isHoveredVertex || isHoveredNeighbour || (filtering && (isMatchedByFilter || isMatchedNeighbour)))
        labelOpactiy = if (!(filtering) || foreground) 1.0 else 0.3
        color = if ((!(hovering || filtering) || foreground)) vertex match {
          case _: AuthorSet => "#FF8A8E"
          case _: PublicationSet => "#48D7FF"
        }
        else vertex match {
          case _: PublicationSet => "#E6F9FF"
          case _: AuthorSet => "#FFE6E6"
        }
      }

      for (e <- graph.edgeData.values) {
        import e._
        val isHoveredIncidentEdge = hovering && (hoveredIncidentEdges contains edge)
        val isIncidentToFilteredVertex = source.isMatchedByFilter || target.isMatchedByFilter

        foreground = isHoveredIncidentEdge
        color = if (hovering || filtering)
          (
            if (isHoveredIncidentEdge || (filtering && isIncidentToFilteredVertex))
              "black" else "#DDDDDD"
          )
        else "#8F8F8F"
      }

      val (fgv, bgv) = graph.vertexData.values.partition(_.foreground)
      val (fge, bge) = graph.edgeData.values.partition(_.foreground)
      drawBgEdges = bge
      drawFgEdges = fge
      drawBgVertices = bgv
      drawFgVertices = fgv
    }

    override def update(p: Props, oldProps: Option[Props] = None) {
      import p._
      import dimensions._

      def newOrChanged(get: Props => Any) = oldProps.isEmpty || get(p) != get(oldProps.get)

      if (newOrChanged(_.simConfig)) {
        simulation.force[PositioningX[VertexInfo]]("gravityx").strength(simConfig.gravity)
        simulation.force[PositioningY[VertexInfo]]("gravityy").strength(simConfig.gravity)
        simulation.force[ManyBody[VertexInfo]]("repel").strength(-simConfig.repel)
        simulation.force[force.Link[EdgeInfo]]("link").distance((e: EdgeInfo) => simConfig.linkDistance / (1 + e.weight))
        simulation.alpha(1).restart()
      }

      if (newOrChanged(_.visConfig.radiusOffset) || newOrChanged(_.visConfig.radiusFactor) || newOrChanged(_.visConfig.radiusExponent)) {
        simulation.force[Collision[VertexInfo]]("collision").radius((v: VertexInfo) => vertexRadius(v, p.visConfig) + collisionGap)
        simulation.alpha(0.1).restart()
      }

      if (newOrChanged(_.visConfig.filter)) {
        updateFilter(p)
        updateHighlight(p)
        draw()
      }

      if (newOrChanged(_.dimensions)) {
        canvas.attr("width", width).attr("height", height)

        simulation.force[Centering]("center").x(width / 2).y(height / 2)
        simulation.force[PositioningX[VertexInfo]]("gravityx").x(width / 2)
        simulation.force[PositioningY[VertexInfo]]("gravityy").y(height / 2)

        simulation.alpha(0.01).restart()
      }

      if (newOrChanged(_.visConfig.authorLabels) || newOrChanged(_.graph)) {
        val authors: Seq[VertexInfo] = graph.vertices.collect { case as: AuthorSet => graph.vertexData(as) }(breakOut)
        val displayedAuthors = authors.sortBy(-_.weight).take((visConfig.authorLabels * authors.size).toInt).toJSArray

        labelsData = labels.selectAll("div").data(displayedAuthors, (vi: VertexInfo) => vi.vertex.asInstanceOf[AuthorSet].ids.mkString)

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
        draw()
      }

      if (newOrChanged(_.graph)) {
        val vertexData = graph.vertices.map(graph.vertexData).toJSArray
        val edgeData = graph.edges.map(graph.edgeData).toJSArray

        simulation.nodes(vertexData)
        simulation.force[force.Link[EdgeInfo]]("link").links(edgeData)

        if (hoveredVertex.isDefined) {
          val hoveredVertexDoesNotExistAnymore = !(vertexData contains hoveredVertex.get)
          if (hoveredVertexDoesNotExistAnymore)
            hoveredVertex = None
        }
        updateHighlight(p)

        simulation.alpha(1).restart()

      }

    }

    def vertexRadius(v: VertexInfo, c: VisualizationConfig) = c.radiusOffset + c.radiusFactor * pow(v.weight, c.radiusExponent)
    def edgeWidth(e: EdgeInfo, c: VisualizationConfig) = c.widthOffset + c.widthFactor * pow(e.weight, c.widthExponent)

    def draw() {
      val p = $.props.runNow()
      import p.dimensions._
      import p.visConfig

      context.save()
      context.clearRect(0, 0, width, height)

      context.translate(transform.x, transform.y)
      context.scale(transform.k, transform.k)

      drawBgEdges.foreach { (e: EdgeInfo) =>
        context.beginPath()
        context.moveTo(e.source.x, e.source.y)
        context.lineTo(e.target.x, e.target.y)
        context.lineWidth = edgeWidth(e, visConfig)
        context.strokeStyle = e.color
        context.stroke()
      }

      drawBgVertices.foreach { (v: VertexInfo) =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig), 0, 2 * Math.PI)
        context.fillStyle = v.color
        context.fill()
      }

      drawFgEdges.foreach { (e: EdgeInfo) =>
        context.beginPath()
        context.moveTo(e.source.x, e.source.y)
        context.lineTo(e.target.x, e.target.y)
        context.lineWidth = edgeWidth(e, visConfig)
        context.strokeStyle = e.color
        context.stroke()
      }

      drawFgVertices.foreach { (v: VertexInfo) =>
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig), 0, 2 * Math.PI)
        context.fillStyle = v.color
        context.fill()
      }

      filterMatchedVertices.foreach { v =>
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig) + filterMatchBorderWidth / 2.0, 0, 2 * Math.PI)
        context.strokeStyle = filterMatchBorderColor
        context.lineWidth = filterMatchBorderWidth
        context.stroke()
      }

      hoveredVertex.foreach { v =>
        context.beginPath()
        context.arc(v.x, v.y, vertexRadius(v, visConfig) + hoverBorderWidth / 2.0, 0, 2 * Math.PI)
        context.strokeStyle = hoverBorderColor
        context.lineWidth = hoverBorderWidth
        context.stroke()
      }

      context.restore()

      labels.selectAll("div")
        .style("left", (v: VertexInfo) => s"${transform.applyX(v.x) - (maxTextWidth / 2)}px")
        .style("top", (v: VertexInfo) => s"${transform.applyY(v.y) - 10}px")
        .style("opacity", (v: VertexInfo) => v.labelOpactiy)
    }

  }

  val backendFactory = new Backend(_)
}
