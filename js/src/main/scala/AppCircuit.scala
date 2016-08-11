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
  filters: Filters = Filters()
) {
  lazy val graph: Graph[PubVertex, DiEdge] = {
    if (publications.publications.isEmpty) Graph.empty
    else {
      val filteredPublications = filters.applyPubFilters(publications)
      println("constructing graph...")
      val fullGraph = filteredPublications.toGraph
      val filteredGraph = filters.applyGraphFilters(fullGraph)
      println(s"displaying ${filteredGraph.nodes.size} vertices...")
      filteredGraph
    }
  }
}

case class SetPublications(publications: Publications) extends Action
case class HoverVertex(v: PubVertex) extends Action
case object UnHoverVertex extends Action
case object UpdateGraph extends Action
case class SetFilters(filter: Filters) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization(
    filters = Filters(
      limit = LimitFilter(100)
    )
  ))

  val publicaitonsHandler = new ActionHandler(zoomRW(_.publicationVisualization)((m, v) => m.copy(publicationVisualization = v))) {
    override def handle = {
      case SetPublications(ps) => updated(value.copy(publications = ps))
      case SetFilters(f) => updated(value.copy(filters = f))
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
