package tigrs

import pharg._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import fdietze.scalajs.react.components.D3ForceLayout

import org.singlespaced.d3js.Ops._
import Math._

object GraphView extends D3ForceLayout[graph.Vertex, SimulationConfig] {
  type V = graph.Vertex
  type G = DirectedGraph[V]

  override def apply(graph: DirectedGraph[V], width: Double, height: Double, props: Option[SimulationConfig]) = component(Props(graph, width, height, props))

  override val panAndZoom = true
  override def chargeDistance(p: Props) = p.props.get.chargeDistance
  override def charge(p: Props, v: V) = {
    import p.graph._
    -p.props.get.charge //* degree(v)
  }
  override def linkDistance(p: Props, e: Edge[V]) = {
    import p.graph._
    p.props.get.linkDistance //* sqrt(abs(degree(e.in) - degree(e.out)) + 1)
  }
  override def linkStrength(p: Props, e: Edge[V]): Double = p.props.get.linkStrength
  override def theta = 0.9
  override def gravity(p: Props) = p.props.get.gravity

  override val vertexElement = "circle"
  override def styleVertices(p: Props, sel: VertexSelection) = {
    sel
      .attr("r", (d: D3Vertex) => log(p.graph.degree(d.v) + 1) * p.props.get.radius)
      // .style("opacity", "0.8")
      .style("fill", { (d: D3Vertex) =>
        d.v match {
          case _: graph.Publication => "#48D7FF"
          case _: graph.PublicationSet => "#48D7FF"
          case _: graph.Author => "#FF8A8E"
          case _: graph.Outlet => "#22E6AB"
          case _: graph.Project => "#D720AF"
          case _: graph.Keyword => "black"
        }
      })
      .on("mouseover", (d: D3Vertex) => AppCircuit.dispatch(HoverVertex(d.v)))
      .on("mouseout", (d: D3Vertex) => AppCircuit.dispatch(UnHoverVertex))

    sel
  }

  override def styleEdges(p: Props, sel: EdgeSelection): EdgeSelection = {
    sel
      .style("stroke", "#8A8A8A")
      .style("stroke-width", 1)
    // .style("opacity", "0.7")
  }

  override def positionVertices(sel: VertexSelection): VertexSelection = {
    sel
      .attr("cx", (d: D3Vertex) => d.x)
      .attr("cy", (d: D3Vertex) => d.y)

    sel
  }

  override def render(p: Props, s: State) = {
    import p._
    <.div(
      ^.position := "absolute",
      ^.top := "0",
      ^.left := "0",
      ^.width := "100%",
      ^.height := "100%",
      ^.zIndex := "-1",
      <.div(
        ^.ref := "container",
        ^.width := "100%",
        ^.height := "100%",
        ^.position := "relative",
        <.svg.svg(
          ^.width := "100%",
          ^.height := "100%",
          ^.position := "absolute",
          <.svg.g(
            ^.ref := "edges"
          ),
          <.svg.g(
            ^.ref := "vertices"
          )
        )
      )
    )
  }
}
