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
  visConfig: VisualizationConfig,
  selectedVertices: Vector[Vertex]
)

object GraphViewCanvas extends CustomComponent[GraphProps]("GraphViewCanvas") {
  val d3js = js.Dynamic.global.d3

  val hoverDistance = 30
  val collisionGap = 2
  val hoverBorderWidth = 5
  val hoverBorderColor = "rgba(200,200,200, 0.7)"
  val filterMatchBorderWidth = 10
  val filterMatchBorderColor = "rgba(136,255,130, 0.7)"
  val selectedBorderWidth = 7
  val selectedBorderColor = "rgba(30,30,30, 0.7)"
  val maxTextWidth = 300

  class Backend($: Scope) extends CustomBackend($) {
    var p: Props = _ //TODO: better solution?

    lazy val container = d3js.select(component)
    lazy val canvas = container.append("canvas")
    lazy val context = canvas.node().asInstanceOf[raw.HTMLCanvasElement].getContext("2d")
    lazy val labels = container.append("div")
    lazy val hoverLabel = container.append("div")
    var labelsData: js.Dynamic = js.undefined.asInstanceOf[js.Dynamic]

    var hoveredVertex: Option[VertexInfo] = None
    var filterMatchedVertices: Iterable[VertexInfo] = Nil
    var drawBgVertices: js.Array[VertexInfo] = js.Array()
    var drawBgEdges: js.Array[EdgeInfo] = js.Array()
    var drawFgVertices: js.Array[VertexInfo] = js.Array()
    var drawFgEdges: js.Array[EdgeInfo] = js.Array()

    val simulation = d3.forceSimulation[VertexInfo]()
      .force("center", d3.forceCenter())
      .force("gravityx", d3.forceX())
      .force("gravityy", d3.forceY())
      .force("repel", d3.forceManyBody())
      .force("link", d3.forceLink())
    // .force("collision", d3.forceCollide())

    simulation.on("tick", () => draw())

    var transform: Transform = d3.zoomIdentity // stores current pan and zoom

    override def init() {
      // init lazy vals
      canvas
      context
      labels
      hoverLabel

      container
        .style("position", "relative")
        .style("overflow", "hidden")

      canvas
        .style("position", "absolute")

      labels
        .style("pointer-events", "none") // pass mouse events to canvas
        .style("text-align", "center")
        .style("text-shadow", "-1px -1px 0 white,  1px -1px 0 white, -1px 1px 0 white, 1px 1px 0 white")

      hoverLabel
        .style("pointer-events", "none") // pass mouse events to canvas
        .style("text-align", "center")
        .style("text-shadow", "-1px -1px 0 white,  1px -1px 0 white, -1px 1px 0 white, 1px 1px 0 white")

      canvas
        .on("mousemove", () => mouseMove())
        .on("mouseout", () => mouseOut())
        .on("click", () => click())
        .call(d3js.zoom().on("zoom", zoomed _))
    }

    def zoomed() {
      transform = d3.event.asInstanceOf[ZoomEvent].transform
      draw()
      hoverLabel.selectAll("div").remove()
    }

    def mouseMove() {
      val pos = transform.invert(d3.mouse(canvas.node().asInstanceOf[raw.HTMLCanvasElement]))

      val d3Vertex = simulation.find(pos(0), pos(1), hoverDistance).toOption
      if (hoveredVertex != d3Vertex) {
        hoveredVertex = d3Vertex.filter(_.isInTimeRange)

        updateHighlight()
        draw()

        hoverLabel.selectAll("div").remove()
        for (v <- hoveredVertex) {
          hoverLabel
            .append("div")
            .style("width", s"${maxTextWidth}px")
            .style("position", "absolute")
            .style("left", s"${transform.applyX(v.x) - (maxTextWidth / 2)}px")
            .style("top", s"${transform.applyY(v.y) - 10}px")
            .text(v.vertex match {
              case AuthorSet(_, as) => as.head.name
              case PublicationSet(_, ps) => ps.head.title
            })
        }
      }
    }

    def mouseOut() {
      if (hoveredVertex != None) {
        hoveredVertex = None
        hoverLabel.selectAll("div").remove()
        // AppCircuit.dispatch(UnHoverVertex)
        updateHighlight()
        draw()
      }
    }

    def click() {
      val pos = transform.invert(d3.mouse(canvas.node().asInstanceOf[raw.HTMLCanvasElement]))

      val d3Vertex = simulation.find(pos(0), pos(1), hoverDistance).toOption
      d3Vertex match {
        case Some(v) =>
          if (v.isSelected) AppCircuit.dispatch(DeselectVertex(v.vertex))
          else AppCircuit.dispatch(SelectVertex(v.vertex))

          v match {
            case VertexInfo(AuthorSet(_, authors), _) => console.log(s"selected:\n ${authors.mkString("\n ")}")
            case VertexInfo(PublicationSet(_, publications), _) => console.log(s"selected:\n ${publications.mkString("\n ")}")
          }
        case None =>
      }
    }

    var filtering: Boolean = false
    def updateFilter() {
      // println("updateFilter")
      val graph = p.graph

      val filter = p.visConfig.filter.trim.toLowerCase
      filtering = filter.nonEmpty
      val matched = mutable.ArrayBuffer.empty[VertexInfo]
      for (v <- graph.vertexData.values) {
        import v._
        isMatchedByFilter = filtering && vertex.isInstanceOf[AuthorSet] && vertex.asInstanceOf[AuthorSet].as.exists(_.name.toLowerCase containsSlice filter)
        if (filtering)
          if (isMatchedByFilter) {
            matched += v
          }
      }
      filterMatchedVertices = matched

      for (v <- graph.vertexData.values) {
        import v._
        isMatchedNeighbour = filtering && vertex.isInstanceOf[PublicationSet] && graph.neighbours(vertex).exists(v => graph.vertexData(v).isMatchedByFilter)
      }
    }

    def updateHighlight() {
      // println("updateHighlight")
      val graph = p.graph

      val hovering = hoveredVertex.isDefined
      val hoveredNeighbours = hoveredVertex.map(v => graph.neighbours(v.vertex)).getOrElse(Set.empty)
      val hoveredIncidentEdges = hoveredVertex.map(v => graph.incidentEdges(v.vertex)).getOrElse(Set.empty)

      for (v <- graph.vertexData.values) {
        import v._
        isHoveredVertex = hovering && (hoveredVertex.get == v)
        isHoveredNeighbour = hovering && (hoveredNeighbours contains vertex)
        isInTimeRange = maxYear >= p.visConfig.minYear && minYear <= p.visConfig.maxYear

        foreground = isInTimeRange && (isHoveredVertex || isHoveredNeighbour || isSelected || (filtering && (isMatchedByFilter || isMatchedNeighbour)))
        labelOpactiy = if (isInTimeRange && !isHoveredVertex) { if (!(filtering) || foreground) 1.0 else 0.3 } else 0.0
        color = if (isInTimeRange) {
          if ((!(hovering || filtering) || foreground)) vertex match {
            case _: AuthorSet => "#FF8A8E"
            case _: PublicationSet => "#48D7FF"
          }
          else vertex match {
            case _: PublicationSet => "#E6F9FF"
            case _: AuthorSet => "#FFE6E6"
          }
        } else "rgba(0,0,0,0)"
        radius = vertexRadius(v, p.visConfig)
        borderWidth = if (isHoveredVertex && isSelected) selectedBorderWidth
        else if (isHoveredVertex) hoverBorderWidth
        else if (isMatchedByFilter) filterMatchBorderWidth
        else if (isSelected) selectedBorderWidth
        else 0.0

        borderColor = if (isHoveredVertex) hoverBorderColor
        else if (isMatchedByFilter) filterMatchBorderColor
        else if (isSelected) selectedBorderColor
        else "#000"
      }

      for (e <- graph.edgeData.values) {
        import e._
        val isHoveredIncidentEdge = hovering && (hoveredIncidentEdges contains edge)
        val isIncidentToFilteredVertex = source.isMatchedByFilter || target.isMatchedByFilter
        val isInTimeRange = source.isInTimeRange && target.isInTimeRange

        foreground = isInTimeRange && isHoveredIncidentEdge
        color = if (isInTimeRange) {
          if (hovering || filtering) {
            if (isHoveredIncidentEdge || (filtering && isIncidentToFilteredVertex))
              "black" else "#DDDDDD"
          } else "#8F8F8F"
        } else "rgba(0,0,0,0)"
        width = edgeWidth(e, p.visConfig)
      }

      val (fgv, bgv) = graph.vertexData.values.partition(_.foreground)
      val (fge, bge) = graph.edgeData.values.partition(_.foreground)
      drawBgEdges = bge.toJSArray
      drawFgEdges = fge.toJSArray
      drawBgVertices = bgv.toJSArray
      drawFgVertices = fgv.toJSArray
    }

    override def update(p: Props, oldProps: Option[Props] = None) {
      // println("update")
      import p._
      import dimensions._
      this.p = p

      def newOrChanged(get: Props => Any) = oldProps.isEmpty || get(p) != get(oldProps.get)
      val simulationHasStopped = simulation.alpha() <= simulation.alphaMin()

      if (newOrChanged(_.simConfig)) {
        simulation.force[PositioningX[VertexInfo]]("gravityx").strength(simConfig.gravity)
        simulation.force[PositioningY[VertexInfo]]("gravityy").strength(simConfig.gravity)
        simulation.force[ManyBody[VertexInfo]]("repel").strength(-simConfig.repel)
        simulation.force[force.Link[VertexInfo, EdgeInfo]]("link").distance((e: EdgeInfo) => simConfig.linkDistance / (1 + e.weight))
        simulation.alpha(1).restart()
      }

      // if (newOrChanged(_.visConfig.radiusOffset) || newOrChanged(_.visConfig.radiusFactor) || newOrChanged(_.visConfig.radiusExponent)) {
      // simulation.force[Collision[VertexInfo]]("collision").radius((v: VertexInfo) => vertexRadius(v, p.visConfig) + collisionGap)
      // simulation.alpha(0.1).restart()
      // }

      if (newOrChanged(_.visConfig.filter)) {
        updateFilter()
        updateHighlight()
        if (simulationHasStopped) draw()
      }

      if (newOrChanged(_.visConfig.minYear) || newOrChanged(_.visConfig.maxYear)) {
        updateHighlight()
        if (simulationHasStopped) draw()
      }

      if (newOrChanged(_.dimensions)) {
        container
          .style("width", s"${width}px")
          .style("height", s"${height}px")

        canvas
          .attr("width", width)
          .attr("height", height)

        simulation.force[Centering[VertexInfo]]("center").x(width / 2).y(height / 2)
        simulation.force[PositioningX[VertexInfo]]("gravityx").x(width / 2)
        simulation.force[PositioningY[VertexInfo]]("gravityy").y(height / 2)

        if (simulationHasStopped)
          simulation.alpha(simulation.alphaMin()).restart()
      }

      if (newOrChanged(_.visConfig.authorLabels) || newOrChanged(_.graph)) {
        val authors: Seq[VertexInfo] = graph.vertices.collect { case as: AuthorSet => graph.vertexData(as) }(breakOut)
        val displayedAuthors = authors.sortBy(-_.weight).take((visConfig.authorLabels * authors.size).toInt).toJSArray

        labelsData = labels.selectAll("div").data(displayedAuthors, (vi: VertexInfo) => vi.vertex.asInstanceOf[AuthorSet].ids.mkString)

        labelsData.enter()
          .append("div")
          .style("width", s"${maxTextWidth}px")
          .style("position", "absolute")
          .text((v: VertexInfo) => v.vertex.asInstanceOf[AuthorSet].as.head.name.split(",")(0))

        labelsData.exit()
          .remove()
      }

      if (newOrChanged(_.visConfig)) {
        updateHighlight()
        draw()
      }

      if (newOrChanged(_.graph)) {
        val vertexData = graph.vertices.map(graph.vertexData).toJSArray
        val edgeData = graph.edges.map(graph.edgeData).toJSArray

        simulation.nodes(vertexData)
        simulation.force[force.Link[VertexInfo, EdgeInfo]]("link").links(edgeData)

        if (hoveredVertex.isDefined) {
          val hoveredVertexDoesNotExistAnymore = !(vertexData contains hoveredVertex.get)
          if (hoveredVertexDoesNotExistAnymore)
            hoveredVertex = None
        }
        updateFilter()
        updateHighlight()

        simulation.alpha(1).restart()

      }

      if (newOrChanged(_.selectedVertices)) {
        for (v <- graph.vertexData.values)
          v.isSelected = false

        for (selected <- p.selectedVertices)
          graph.vertexData(selected).isSelected = true

        updateHighlight()
        draw()
      }

    }

    def vertexRadius(v: VertexInfo, c: VisualizationConfig) = c.radiusOffset + c.radiusFactor * pow(v.weight, c.radiusExponent)
    def edgeWidth(e: EdgeInfo, c: VisualizationConfig) = c.widthOffset + c.widthFactor * pow(e.weight, c.widthExponent)

    val fullDeg = 2 * Math.PI
    def draw() {
      context.save()
      context.clearRect(0, 0, p.dimensions.width, p.dimensions.height)

      context.translate(transform.x, transform.y)
      context.scale(transform.k, transform.k)
      var i = 0

      i = 0
      while (i < drawBgEdges.size) {
        val e = drawBgEdges(i)
        context.beginPath()
        context.moveTo(e.source.x, e.source.y)
        context.lineTo(e.target.x, e.target.y)
        context.lineWidth = e.width
        context.strokeStyle = e.color
        context.stroke()
        i += 1
      }

      i = 0
      while (i < drawBgVertices.size) {
        val v = drawBgVertices(i)
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, v.radius, 0, fullDeg)
        context.fillStyle = v.color
        context.fill()
        i += 1
      }

      i = 0
      while (i < drawFgEdges.size) {
        val e = drawFgEdges(i)
        context.beginPath()
        context.moveTo(e.source.x, e.source.y)
        context.lineTo(e.target.x, e.target.y)
        context.lineWidth = e.width
        context.strokeStyle = e.color
        context.stroke()
        i += 1
      }

      i = 0
      while (i < drawFgVertices.size) {
        val v = drawFgVertices(i)
        context.moveTo(v.x, v.y)
        context.beginPath()
        context.arc(v.x, v.y, v.radius, 0, fullDeg)
        context.fillStyle = v.color
        context.fill()
        if (v.borderWidth > 0.0) {
          context.beginPath()
          context.arc(v.x, v.y, v.radius + v.borderWidth / 2.0, 0, fullDeg)
          context.strokeStyle = v.borderColor
          context.lineWidth = v.borderWidth
          context.stroke()
        }
        i += 1
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
