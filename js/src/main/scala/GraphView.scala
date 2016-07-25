package tigrs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import fdietze.scalajs.react.components.D3ForceLayout

import org.singlespaced.d3js.Ops._

object GraphView extends D3ForceLayout[PubVertex, DiEdge] {
  override val panAndZoom = true
  override val vertexElement = "g"
  override def styleVertices(sel: VertexSelection) = {
    sel.append("circle")
      .attr("r", (d: D3Vertex) => 5)
      .style("fill", (d: D3Vertex) => d.v match {
        case _: Publication => "#4C90EB"
        case _: Author => "#DB6F45"
        case _: Outlet => "#20D74D"
        case _: Project => "#D720AF"
        case _: Keyword => "black"
      })
      .on("mouseover", (d: D3Vertex) => AppCircuit.dispatch(HoverVertex(d.v)))
      .on("mouseout", (d: D3Vertex) => AppCircuit.dispatch(UnHoverVertex))
    // .append("svg:title")
    // .text((d: D3Vertex) => d.v match {
    //   case p: Publication => p.title
    //   case a: Author => a.name
    //   case o: Outlet => o.name
    // })

    // sel.append("text")
    //   .text((d: D3Vertex) => d.v match {
    //     case p: Publication => p.title.split(" ").head
    //     case a: Author => a.name
    //     case o: Outlet => o.name
    //   })
    //   .style("font-family", (d: D3Vertex) => "sans-serif")
    //   .style("font-size", (d: D3Vertex) => "5px")
    sel
  }

  override def positionVertices(sel: VertexSelection): VertexSelection = {
    sel.select("circle")
      .attr("cx", (d: D3Vertex) => d.x)
      .attr("cy", (d: D3Vertex) => d.y)

    // sel.select("text")
    //   .attr("x", (d: D3Vertex) => d.x)
    //   .attr("y", (d: D3Vertex) => d.y)

    sel
  }
}
