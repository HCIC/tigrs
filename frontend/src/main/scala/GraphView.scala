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

import org.singlespaced.d3js.d3
import org.singlespaced.d3js
import org.singlespaced.d3js.Ops._
import Math._

case class GraphConfig(
  simConfig: SimulationConfig,
  hovered: Option[graph.Vertex] = None,
  highlightedVertices: Set[graph.Vertex] = Set.empty
)

object GraphViewCanvas extends D3[DirectedGraph[graph.Vertex]]("GraphViewCanvas") {
  @js.native
  trait D3V extends js.Object {
    var x: js.UndefOr[Double] = js.native
    var y: js.UndefOr[Double] = js.native
  }

  class D3E(
    val source: D3V,
    val target: D3V
  ) extends d3js.Link[D3V]

  class Backend($: Scope) extends D3Backend($) {
    lazy val canvas = component.append("canvas")
    lazy val context = canvas.node().asInstanceOf[raw.HTMLCanvasElement].getContext("2d")
    val width = 900.0
    val height = 900.0

    val force = d3.layout.force[D3V, D3E]().size((width, height))

    force.on("tick", (e: Event) => {
      updateVisualization()
    })

    override def update(p: Props) = Callback {
      canvas.attr("width", width).attr("height", height)

      val vertexData = p.vertices.toJSArray
      val edgeData = p.edges.map(e => new D3E(e.in.asInstanceOf[D3V], e.out.asInstanceOf[D3V])).toJSArray

      force.nodes(vertexData.asInstanceOf[js.Array[D3V]]).links(edgeData)
      force.start()
    }

    def updateVisualization() {
      context.clearRect(0, 0, width, height)

      // draw links
      context.strokeStyle = "#8F8F8F"
      context.beginPath()
      force.links.foreach { d =>
        context.moveTo(d.source.x, d.source.y)
        context.lineTo(d.target.x, d.target.y)
      }
      context.stroke()

      // draw nodes
      force.nodes.foreach { d =>
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

object GraphView extends D3[DirectedGraph[graph.Vertex]]("GraphView") {
  @js.native
  trait D3V extends js.Object {
    var x: js.UndefOr[Double] = js.native
    var y: js.UndefOr[Double] = js.native
  }

  class D3E(
    val source: D3V,
    val target: D3V
  ) extends d3js.Link[D3V]

  class Backend($: Scope) extends D3Backend($) {
    lazy val svg = component.append("svg")
    lazy val vertices = svg.append("g")
    lazy val edges = svg.append("g")
    val force = d3.layout.force[D3V, D3E]().size((600.0, 600.0))

    force.on("tick", (e: Event) => {
      updateVisualization()
    })

    override def update(p: Props) = Callback {
      svg.attr("width", 600).attr("height", 600)

      // edges first => lazy val is evaluated first => g-element is appended before vertices
      // => edges stay in background
      val edgeData = p.edges.map(e => new D3E(e.in.asInstanceOf[D3V], e.out.asInstanceOf[D3V])).toJSArray
      val edge = edges.selectAll("line").data(edgeData)

      val vertexData = p.vertices.toJSArray
      val vertex = vertices.selectAll("circle").data(vertexData)

      vertex.enter()
        .append("circle")

      vertex
        .attr("r", 5)
        .attr("fill", { (d: graph.Vertex) =>
          d match {
            case _: graph.Publication => "#48D7FF"
            case _: graph.PublicationSet => "#48D7FF"
            case _: graph.Author => "#FF8A8E"
            case _: graph.AuthorSet => "#FF8A8E"
            case _: graph.Outlet => "#22E6AB"
            case _: graph.Project => "#D720AF"
            case _: graph.Keyword => "black"
          }
        })

      vertex.exit()
        .remove()

      edge.enter()
        .append("line")

      edge
        .attr("stroke", "#8F8F8F")

      edge.exit()
        .remove()

      force.nodes(vertexData.asInstanceOf[js.Array[D3V]]).links(edgeData)
      force.start()
    }

    def updateVisualization() {
      val vertex = vertices.selectAll("circle")
      val edge = edges.selectAll("line")

      vertex
        .attr("cx", (d: graph.Vertex) => d.asInstanceOf[D3V].x)
        .attr("cy", (d: graph.Vertex) => d.asInstanceOf[D3V].y)

      edge
        .attr("x1", (d: D3E) => d.source.asInstanceOf[D3V].x)
        .attr("y1", (d: D3E) => d.source.asInstanceOf[D3V].y)
        .attr("x2", (d: D3E) => d.target.asInstanceOf[D3V].x)
        .attr("y2", (d: D3E) => d.target.asInstanceOf[D3V].y)
    }

  }

  val backendFactory = new Backend(_)
}

object GraphViewOld extends D3ForceLayout[graph.Vertex, GraphConfig] {
  type V = graph.Vertex
  type G = DirectedGraph[V]

  override def apply(graph: DirectedGraph[V], dim: Vec2, props: Option[GraphConfig]) = component(Props(graph, dim, props))

  override val panAndZoom = true
  override def chargeDistance(p: Props) = p.props.simConfig.chargeDistance
  override def charge(p: Props, v: V) = {
    import p.graph._
    import p.props.{simConfig => c}
    -c.charge //* degree(v)
  }
  override def linkDistance(p: Props, e: Edge[V]) = {
    import p.graph._
    import p.props.{simConfig => c}
    c.linkDistance //* sqrt(abs(degree(e.in) - degree(e.out)) + 1)
  }
  override def linkStrength(p: Props, e: Edge[V]): Double = p.props.simConfig.linkStrength
  override def theta = 0.9
  override def gravity(p: Props) = p.props.simConfig.gravity

  override def shouldUpdateForce(current: Props, next: Props) = {
    import current.props.{simConfig => c}
    import next.props.{simConfig => n}
    (c.charge != n.charge) ||
      (c.chargeDistance != n.chargeDistance) ||
      (c.linkDistance != n.linkDistance) ||
      (c.linkStrength != n.linkStrength) ||
      (c.gravity != n.gravity)
  }
  override def shouldUpdateVisualization(current: Props, next: Props) = {
    (current.props.simConfig.radius != next.props.simConfig.radius) ||
      (current.props.hovered != next.props.hovered)
  }

  override val vertexElement = "g"
  override def styleVertices(p: Props, sel: VertexSelection) = {
    import p.graph._
    import p.props.highlightedVertices
    import p.props.hovered
    import p.props.simConfig

    sel.selectAll("*").remove() //TODO: replace elements more efficiently

    sel
      .append("circle")
      .attr("r", (d: D3Vertex) => d.v match {
        case _: graph.Author => log(p.graph.degree(d.v) + 1) * simConfig.radius
        case ps: graph.PublicationSet => sqrt(ps.ids.size) * simConfig.radius
        case as: graph.AuthorSet => (sqrt(as.ids.size) + sqrt(p.graph.degree(as))) * simConfig.radius
        case _ => simConfig.radius
      })
      .on("mouseover", (d: D3Vertex) => AppCircuit.dispatch(HoverVertex(d.v)))
      .on("mouseout", (d: D3Vertex) => AppCircuit.dispatch(UnHoverVertex))
      .style("fill", { (d: D3Vertex) =>
        d.v match {
          case _: graph.Publication => "#48D7FF"
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.Author => "#FF8A8E"
          case _: graph.AuthorSet => "#FF8A8E"
          case _: graph.Outlet => "#22E6AB"
          case _: graph.Project => "#D720AF"
          case _: graph.Keyword => "black"
        }
      })
      .style("stroke", (d: D3Vertex) => { if (hovered.isEmpty) "#8F8F8F" else { if (hovered.get == d.v || highlightedVertices.contains(d.v)) "black" else "#8F8F8F" } })
      .style("opacity", (d: D3Vertex) => { if (hovered.isEmpty || hovered.get == d.v || highlightedVertices.contains(d.v)) "1.0" else "0.3" }) // || p.graph.neighbours(d.v).contains(hovered.get)

    sel
      .append("text")
      .text((d: D3Vertex) => {
        d.v match {
          case p: graph.Publication if ((hovered.isDefined && hovered.get == d.v) || highlightedVertices.contains(d.v)) => p.p.title.toString
          case p: graph.PublicationSet if ((hovered.isDefined && hovered.get == d.v) || highlightedVertices.contains(d.v)) => p.ps.map(p => p.title.take(25) + "...").mkString(" / ")
          case a: graph.Author if (degree(d.v) >= 5 || (hovered.isDefined && hovered.get == d.v) || highlightedVertices.contains(d.v)) => a.a.name.toString
          case a: graph.AuthorSet if (degree(d.v) >= 5 || (hovered.isDefined && hovered.get == d.v) || highlightedVertices.contains(d.v)) => a.as.map(_.name).mkString(" / ")
          case _ => ""
        }
      })
      .style("opacity", (d: D3Vertex) => { if (hovered.isEmpty || hovered.get == d.v || highlightedVertices.contains(d.v)) "1.0" else "0.0" }) // || p.graph.neighbours(d.v).contains(p.props.hovered.get)

    sel
  }

  override def styleEdges(p: Props, sel: EdgeSelection): EdgeSelection = {
    sel
      .style("stroke", (d: D3Edge) => { if (p.props.hovered.isEmpty) "#8F8F8F" else if (p.props.hovered.exists(h => h == d.e.in || h == d.e.out)) "black" else "#8F8F8F" })
      .style("stroke-width", 1)
      .style("opacity", (d: D3Edge) => { if (p.props.hovered.isEmpty) "1.0" else if (p.props.hovered.exists(h => h == d.e.in || h == d.e.out)) "1.0" else "0.3" })
  }

  override def positionVertices(sel: VertexSelection): VertexSelection = {
    sel
      // .attr("x", (d: D3Vertex) => d.x)
      // .attr("y", (d: D3Vertex) => d.y)
      .attr("transform", (d: D3Vertex) => s"translate(${d.x},${d.y})")
    // .style("transform", (d: D3Vertex) => s"translate(${d.x}px,${d.y}px)")

    sel
  }
}
