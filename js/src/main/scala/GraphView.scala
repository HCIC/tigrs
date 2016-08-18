package tigrs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import fdietze.scalajs.react.components.D3ForceLayout

import org.singlespaced.d3js.Ops._

object GraphView extends D3ForceLayout[PubVertex, DiEdge] {
  override val panAndZoom = true
  // override def chargeDistance = 300
  override def theta = 0.9
  override val vertexElement = "circle"
  override def styleVertices(sel: VertexSelection) = {
    sel
      .attr("r", (d: D3Vertex) => 5)
      .style("fill", { (d: D3Vertex) =>
        d.v match {
          case _: Publication => "#4C90EB"
          case _: Author => "#DB6F45"
          case _: Outlet => "#20D74D"
          case _: Project => "#D720AF"
          case _: Keyword => "black"
        }
      })
      .on("mouseover", (d: D3Vertex) => AppCircuit.dispatch(HoverVertex(d.v)))
      .on("mouseout", (d: D3Vertex) => AppCircuit.dispatch(UnHoverVertex))

    sel
  }

  override def positionVertices(sel: VertexSelection): VertexSelection = {
    sel
      .attr("cx", (d: D3Vertex) => d.x)
      .attr("cy", (d: D3Vertex) => d.y)

    sel
  }
}
