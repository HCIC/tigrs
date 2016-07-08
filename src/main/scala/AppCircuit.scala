package tigrs

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

case class RootModel(graph: Graph[PubVertex, DiEdge])
case class SetGraph(graph: Graph[PubVertex, DiEdge]) extends Action
case class AddVertex(v: PubVertex) extends Action
case class AddEdge(e: DiEdge[PubVertex]) extends Action
case class RemoveVertex(v: PubVertex) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(Graph.empty)

  val graphHandler = new ActionHandler(zoomRW(_.graph)((m, v) => m.copy(graph = v))) {
    override def handle = {
      case SetGraph(g) => updated(g)
      case AddVertex(v) => updated(value + v)
      case AddEdge(e) => updated(value + e)
      case RemoveVertex(v) => updated(value - v)
    }
  }
  val actionHandler = composeHandlers(graphHandler)
}
