package tigrs

import pharg._
import vectory._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import fdietze.scalajs.react.components.D3ForceLayout

import org.singlespaced.d3js.Ops._
import Math._

case class GraphConfig(
  simConfig: SimulationConfig,
  hovered: Option[graph.Vertex] = None,
  highlightedVertices: Set[graph.Vertex] = Set.empty
)

object GraphView extends D3ForceLayout[graph.Vertex, GraphConfig] {
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
    sel.selectAll("*").remove() //TODO: replace elements more efficiently

    sel
      .append("circle")
      .attr("r", (d: D3Vertex) => d.v match {
        case _: graph.Author => log(p.graph.degree(d.v) + 1) * p.props.simConfig.radius
        case ps: graph.PublicationSet => sqrt(ps.ids.size) * p.props.simConfig.radius
        case as: graph.AuthorSet => (sqrt(as.ids.size) + sqrt(p.graph.degree(as))) * p.props.simConfig.radius
        case _ => p.props.simConfig.radius
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
      .style("stroke", (d: D3Vertex) => { if (p.props.hovered.isEmpty) "#8F8F8F" else { if (p.props.hovered.get == d.v || p.props.highlightedVertices.contains(d.v)) "black" else "#8F8F8F" } })
      .style("opacity", (d: D3Vertex) => { if (p.props.hovered.isEmpty || p.props.hovered.get == d.v || p.props.highlightedVertices.contains(d.v)) "1.0" else "0.3" }) // || p.graph.neighbours(d.v).contains(p.props.hovered.get)

    sel
      .append("text")
      .text((d: D3Vertex) => d.v match {
        case p: graph.Publication => p.id.toString
        case p: graph.PublicationSet => p.ids.mkString("\n")
        case a: graph.Author => a.id.toString
        case a: graph.AuthorSet => a.ids.mkString("\n")
        case v => ""
      })
      .style("opacity", (d: D3Vertex) => { if (p.props.hovered.isEmpty || p.props.hovered.get == d.v || p.props.highlightedVertices.contains(d.v)) "1.0" else "0.0" }) // || p.graph.neighbours(d.v).contains(p.props.hovered.get)

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
