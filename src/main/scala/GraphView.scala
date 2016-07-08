package tigrs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import fdietze.scalajs.react.components.D3ForceLayout

import org.singlespaced.d3js.Ops._

object GraphView extends D3ForceLayout[PubVertex, DiEdge] {
  override def styleVertices(sel: VertexSelection) = {
    super.styleVertices(sel)
      .style("fill", (d: D3Vertex) => d.v match {
        case _: Publication => "#4C90EB"
        case _: Author => "#DB6F45"
      })
      .attr("title", (d: D3Vertex) => d.v match {
        case p: Publication => p.title
        case a: Author => a.name
      })
  }
}
