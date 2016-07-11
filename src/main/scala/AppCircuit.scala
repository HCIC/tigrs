package tigrs

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

case class RootModel(graph: Graph[PubVertex, DiEdge] = Graph.empty, hoveredVertex: Option[PubVertex] = None)
case class SetGraph(graph: Graph[PubVertex, DiEdge]) extends Action
case class HoverVertex(v: PubVertex) extends Action
case object UnHoverVertex extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel()

  val graphHandler = new ActionHandler(zoomRW(_.graph)((m, v) => m.copy(graph = v))) {
    override def handle = {
      case SetGraph(g) => updated(g)
    }
  }
  val previewHandler = new ActionHandler(zoomRW(_.hoveredVertex)((m, v) => m.copy(hoveredVertex = v))) {
    override def handle = {
      case HoverVertex(v) => updated(Some(v))
      case UnHoverVertex => updated(None)
    }
  }
  val actionHandler = composeHandlers(graphHandler, previewHandler)
}
