package tigrs

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._
import pharg._
import vectory._

import cats.Monoid

import scala.concurrent.ExecutionContext.Implicits.global

case class RootModel(
  publicationVisualization: PublicationVisualization,
  hoveredVertex: Option[graph.Vertex] = None,
  highlightedVertices: Set[graph.Vertex] = Set.empty,
  preview: Option[AnyRef] = None
)

case class Search(title: String = "") {
  def isEmpty = title.isEmpty
}

case class SimulationConfig(
  radius: Double = 3,
  charge: Double = 550,
  chargeDistance: Double = 999,
  linkDistance: Double = 5,
  linkStrength: Double = 1,
  gravity: Double = 0.15,
  pubSimilarity: Double = 1.0,
  authorSimilarity: Double = 1.0
)

case class PublicationVisualization(
  search: Search = Search(),
  filters: Filters = Filters(),
  graph: DirectedGraph[tigrs.graph.Vertex] = Monoid[DirectedGraph[tigrs.graph.Vertex]].empty,
  dimensions: Vec2 = Vec2(100, 100),
  config: SimulationConfig = SimulationConfig(),
  sliderWidget: Boolean = false
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
case class SetPreview(d: AnyRef) extends Action
case object UnHoverVertex extends Action
// case object UpdateGraph extends Action
case class SetSearch(search: Search) extends Action
case class SetFilters(filter: Filters) extends Action
case class SetGraph(graph: DirectedGraph[tigrs.graph.Vertex]) extends Action
case class SetDimensions(dimensions: Vec2) extends Action
case class DownloadGraph(url: String) extends Action
case class SetConfig(config: SimulationConfig) extends Action
case class ShowSliderWidget(show: Boolean) extends Action

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
      case SetDimensions(dim) => updated(value.copy(dimensions = dim))
      case DownloadGraph(url) => effectOnly(Effect {
        Database.downloadGraph(url).map {
          graph => (SetGraph(graph))
        }
      })
      case SetConfig(c) => updated(value.copy(config = c))
      case ShowSliderWidget(show) => updated(value.copy(sliderWidget = show))
    }
  }
  val previewHandler = new ActionHandler(zoomRW(m => m)((m, v) => v)) {
    override def handle = {
      case HoverVertex(v) => updated(
        value.copy(hoveredVertex = Some(v), highlightedVertices = value.publicationVisualization.graph.neighbours(v)),
        Effect(v match {
          case graph.Publication(recordId, _) => Database.lookupPublication(recordId).map(p => SetPreview(p))
          case graph.PublicationSet(recordIds, _) => Database.lookupPublications(recordIds).map(ps => SetPreview(PublicationSeq(ps)))
          case graph.Author(id,_) => Database.lookupAuthor(id).map(p => SetPreview(p))
          case graph.AuthorSet(ids,_) => Database.lookupAuthors(ids).map(as => SetPreview(AuthorSeq(as)))
          case other =>
            println(other)
            concurrent.Future { NoAction }
        })
      )
      case SetPreview(v) => updated(value.copy(preview = Some(v)))
      case UnHoverVertex => updated(value.copy(hoveredVertex = None, preview = None))
    }
  }
  val actionHandler = composeHandlers(publicaitonsHandler, previewHandler)
}
