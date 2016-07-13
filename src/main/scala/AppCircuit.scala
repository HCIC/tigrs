package tigrs

import org.scalajs.dom

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

case class RootModel(
  faculty: String = Global.faculties.head,
  graph: Graph[PubVertex, DiEdge] = Graph.empty,
  hoveredVertex: Option[PubVertex] = None
)

case class SetFaculty(faculty: String) extends Action
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
  val modelHandler = new ActionHandler(zoomRW(m => m)((m, v) => v)) {
    override def handle = {
      case SetFaculty(f) if Global.faculties contains f =>

        val xhr = new dom.XMLHttpRequest()
        xhr.open("GET", s"data/$f.xml")
        xhr.onload = { (e: dom.Event) =>
          if (xhr.status == 200) {
            val publications = ModsParser.xmlToPublications(xhr.responseXML, Global.publicationLimit)
            AppCircuit.dispatch(SetGraph(publications.toGraph))
          }
        }
        xhr.send()
        updated(value.copy(faculty = f))
    }
  }
  val actionHandler = composeHandlers(graphHandler, previewHandler, modelHandler)
}
