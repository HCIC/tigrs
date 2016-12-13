package tigrs

import org.scalajs.dom._
import scala.scalajs.js
import scala.scalajs.js.annotation._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._
import pharg._
import vectory._

import cats.Monoid

import scala.concurrent.ExecutionContext.Implicits.global
import concurrent.Future

case class RootModel(
  publicationVisualization: PublicationVisualization,
  hoveredVertex: Option[graph.Vertex] = None
)

case class Search(title: String = "") {
  def isEmpty = title.isEmpty
}

case class SimulationConfig(
  radius: Double = 3,
  charge: Double = 550,
  chargeDistance: Double = 2000,
  linkDistance: Double = 5,
  linkStrength: Double = 1,
  gravity: Double = 0.15,
  pubSimilarity: Double = 1.0,
  authorSimilarity: Double = 1.0
)

case class PublicationVisualization(
  search: Search = Search(),
  // filters: Filters = Filters(),
  ikzs: Seq[String] = Nil,
  ikz: Option[String] = None,
  publications: Seq[Publication] = Nil,
  displayGraph: DirectedGraph[tigrs.graph.Vertex] = Monoid[DirectedGraph[tigrs.graph.Vertex]].empty,
  dimensions: Vec2 = Vec2(100, 100),
  config: SimulationConfig = SimulationConfig(),
  sliderWidget: Boolean = false
) {
}

case class HoverVertex(v: graph.Vertex) extends Action
case object UnHoverVertex extends Action
// case class SetFilters(filter: Filters) extends Action
case class SetDisplayGraph(graph: DirectedGraph[tigrs.graph.Vertex]) extends Action
case class SetDimensions(dimensions: Vec2) extends Action
case class DownloadPublications(url: String) extends Action
case class SetPublications(ps: Seq[Publication]) extends Action
case class SetIkzList(ikzs: Seq[String]) extends Action
case class SetIkz(ikz: String) extends Action
case class SetConfig(config: SimulationConfig) extends Action
case class ShowSliderWidget(show: Boolean) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization())

  val publicaitonsHandler = new ActionHandler(zoomRW(_.publicationVisualization)((m, v) => m.copy(publicationVisualization = v))) {
    override def handle = {
      // case SetFilters(f) => updated(value.copy(filters = f))
      case SetDisplayGraph(g) =>
        console.log(s"displaying graph with ${g.vertices.size} vertices and ${g.edges.size} edges.")
        updated(value.copy(displayGraph = g))
      case SetPublications(ps) => updated(value.copy(publications = ps), Effect(Future { SetDisplayGraph(tigrs.graph.mergedGraph(value.config.pubSimilarity, value.config.authorSimilarity)(ps)) }))
      case SetIkzList(ikzs) => updated(value.copy(ikzs = ikzs))
      case SetIkz(ikz) => updated(value.copy(ikz = Some(ikz)), Effect { Future.successful(DownloadPublications(s"fakall.ikz.$ikz")) })
      case SetDimensions(dim) => updated(value.copy(dimensions = dim))
      case DownloadPublications(url) => effectOnly(Effect {
        Data.downloadPublications(url).map {
          graph => (SetPublications(graph))
        }
      })
      case SetConfig(c) => updated(value.copy(config = c))
      case ShowSliderWidget(show) => updated(value.copy(sliderWidget = show))
    }
  }
  val previewHandler = new ActionHandler(zoomRW(m => m)((m, v) => v)) {
    override def handle = {
      case HoverVertex(v) => updated(
        value.copy(hoveredVertex = Some(v))
      )
      case UnHoverVertex => updated(value.copy(hoveredVertex = None))
    }
  }
  val actionHandler = composeHandlers(publicaitonsHandler, previewHandler)
}
