package tigrs

import org.scalajs.dom

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

case class RootModel(
  publicationVisualization: PublicationVisualization,
  hoveredVertex: Option[PubVertex] = None
)

case class PublicationVisualization(
  publications: Publications = Publications(Nil),
  filters: Seq[GraphFilter] = Nil
) {
  lazy val graph: Graph[PubVertex, DiEdge] = {
    println("constructing graph...")
    val fullGraph = publications.toGraph
    println("applying graph filters...")
    val g = filters.foldLeft(fullGraph) { (g, f) => println(f.getClass.getName); f(g) }
    println(s"displaying ${g.nodes.size} vertices...")
    g
  }
}

case class SetPublications(publications: Publications) extends Action
case class HoverVertex(v: PubVertex) extends Action
case object UnHoverVertex extends Action
case object UpdateGraph extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization(filters = List(MinDegreeFilter(7))))

  val publicaitonsHandler = new ActionHandler(zoomRW(_.publicationVisualization)((m, v) => m.copy(publicationVisualization = v))) {
    override def handle = {
      case SetPublications(ps) => updated(value.copy(publications = ps))
    }
  }
  val previewHandler = new ActionHandler(zoomRW(_.hoveredVertex)((m, v) => m.copy(hoveredVertex = v))) {
    override def handle = {
      case HoverVertex(v) => updated(Some(v))
      case UnHoverVertex => updated(None)
    }
  }
  val actionHandler = composeHandlers(publicaitonsHandler, previewHandler)
}
