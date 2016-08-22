package tigrs

import org.scalajs.dom

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import scala.concurrent.ExecutionContext.Implicits.global

case class RootModel(
  publicationVisualization: PublicationVisualization,
  hoveredVertex: Option[PubVertex] = None
)

case class Search(title: String = "") {
  def isEmpty = title.isEmpty
}

case class PublicationVisualization(
  search: Search = Search(),
  filters: Filters = Filters(),
  graph: Graph[PubVertex, DiEdge] = Graph.empty
) {
  // lazy val graph: Graph[PubVertex, DiEdge] = {
  //   import Database._
  //   if (search.isEmpty) Graph.empty
  //   else {
  //     val publications = Database.search(search)
  //     val filteredPublications = filters.applyPubFilters(publications)
  //     println("constructing graph...")
  //     val fullGraph = filteredPublications.toGraph
  //     val filteredGraph = filters.applyGraphFilters(fullGraph)
  //     println(s"displaying ${filteredGraph.nodes.size} vertices...")
  //     filteredGraph
  //   }
  // }
}

case class HoverVertex(v: PubVertex) extends Action
case object UnHoverVertex extends Action
// case object UpdateGraph extends Action
case class SetSearch(search: Search) extends Action
case class SetFilters(filter: Filters) extends Action
case class SetGraph(graph: Graph[PubVertex, DiEdge]) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization(filters = Filters(
    limit = LimitFilter(100)
  )))

  val publicaitonsHandler = new ActionHandler(zoomRW(_.publicationVisualization)((m, v) => m.copy(publicationVisualization = v))) {
    override def handle = {
      case SetSearch(s) => updated(value.copy(search = s), {
        val search = Database.search(s)
        search.onFailure { case e => throw e }
        Effect(search.map { publications =>
          val filteredPublications = value.filters.applyPubFilters(publications)
          println(s"constructing graph from ${filteredPublications.publications.size} publications...")
          val fullGraph = filteredPublications.toGraph
          val filteredGraph = value.filters.applyGraphFilters(fullGraph)
          println(s"displaying ${filteredGraph.nodes.size} vertices...")
          SetGraph(filteredGraph)
        })
      })
      case SetFilters(f) => updated(value.copy(filters = f))
      case SetGraph(g) => updated(value.copy(graph = g))
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
