package tigrs

import org.scalajs.dom

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._
import pharg._

import cats.Monoid

import scala.concurrent.ExecutionContext.Implicits.global

case class RootModel(
  publicationVisualization: PublicationVisualization,
  hoveredVertex: Option[AnyRef] = None
)

case class Search(title: String = "") {
  def isEmpty = title.isEmpty
}

case class SimulationConfig(
  radius: Double = 3,
  charge: Double = 430,
  chargeDistance: Double = 220,
  linkDistance: Double = 5,
  linkStrength: Double = 1,
  gravity: Double = 0.15,
  pubSimilarity: Double = 0.4,
  authorSimilarity: Double = 0.3
)

case class PublicationVisualization(
  search: Search = Search(),
  filters: Filters = Filters(),
  graph: DirectedGraph[tigrs.graph.Vertex] = Monoid[DirectedGraph[tigrs.graph.Vertex]].empty,
  config: SimulationConfig = SimulationConfig()
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

case class HoverVertex(v: graph.Vertex) extends Action
case class SetHoveredVertex(v: AnyRef) extends Action
case object UnHoverVertex extends Action
// case object UpdateGraph extends Action
case class SetSearch(search: Search) extends Action
case class SetFilters(filter: Filters) extends Action
case class SetGraph(graph: DirectedGraph[tigrs.graph.Vertex]) extends Action
case class DownloadGraph(url: String) extends Action
case class SetConfig(config: SimulationConfig) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization(filters = Filters( // limit = LimitFilter(100)
  )))

  val publicaitonsHandler = new ActionHandler(zoomRW(_.publicationVisualization)((m, v) => m.copy(publicationVisualization = v))) {
    override def handle = {
      case SetSearch(s) => updated(value.copy(search = s), {
        val search = Database.search(s)
        search.onFailure { case e => throw e }
        Effect(search.map { publications =>
          println(s"filtering ${publications.size} publications...")
          val filteredPublications = value.filters.applyPubFilters(publications)
          println(s"constructing graph from ${filteredPublications.size} publications...")
          val fullGraph = graph.pubGraph(filteredPublications)
          val filteredGraph = value.filters.applyGraphFilters(fullGraph)
          println(s"displaying ${filteredGraph.vertices.size} vertices...")
          SetGraph(filteredGraph)
        })
      })
      case SetFilters(f) => updated(value.copy(filters = f))
      case SetGraph(g) => updated(value.copy(graph = g))
      case DownloadGraph(url) => effectOnly(Effect {
        Database.downloadGraph(url).map {
          graph => (SetGraph(graph))
        }
      })
      case SetConfig(c) => updated(value.copy(config = c))
    }
  }
  val previewHandler = new ActionHandler(zoomRW(_.hoveredVertex)((m, v) => m.copy(hoveredVertex = v))) {
    override def handle = {
      case HoverVertex(v) => effectOnly(Effect(v match {
        case graph.Publication(recordId) => Database.lookupPublication(recordId).map(p => SetHoveredVertex(p))
        case graph.PublicationSet(recordIds) => Database.lookupPublications(recordIds).map(ps => SetHoveredVertex(PublicationSeq(ps)))
        case graph.Author(id) => Database.lookupAuthor(id).map(p => SetHoveredVertex(p))
        case graph.AuthorSet(ids) => Database.lookupAuthors(ids).map(as => SetHoveredVertex(AuthorSeq(as)))
        case other =>
          println(other)
          concurrent.Future { NoAction }
      }))
      case SetHoveredVertex(v) => updated(Some(v))
      case UnHoverVertex => updated(None)
    }
  }
  val actionHandler = composeHandlers(publicaitonsHandler, previewHandler)
}
