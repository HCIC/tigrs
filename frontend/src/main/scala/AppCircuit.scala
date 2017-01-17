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

import collection.breakOut

import graph._

case class RootModel(
  publicationVisualization: PublicationVisualization,
  hoveredVertex: Option[graph.Vertex] = None,
  selectedVertices: Vector[graph.Vertex] = Vector.empty
)

case class Search(title: String = "") {
  def isEmpty = title.isEmpty
}

sealed trait Config

case class GraphConfig(
  pubSimilarity: Double = 1.0,
  authorSimilarity: Double = 1.0,
  fractionalCounting: Boolean = true
) extends Config

case class SimulationConfig(
  repel: Double = 30,
  gravity: Double = 0.03,
  linkDistance: Double = 30
) extends Config

case class VisualizationConfig(
  minYear: Double = ((new java.util.Date()).getYear + 1900) - 30,
  maxYear: Double = ((new java.util.Date()).getYear + 1900),
  radiusOffset: Double = 1.0,
  radiusFactor: Double = 1.8,
  radiusExponent: Double = 0.608,
  widthOffset: Double = 0.1,
  widthFactor: Double = 0.7,
  widthExponent: Double = 1.184,
  authorLabels: Double = 0.1,
  filter: String = ""
) extends Config

case class PublicationVisualization(
  availableIkzs: Seq[String] = Nil,
  selectedIkzs: Set[String] = Set.empty,
  publications: Seq[Publication] = Nil,
  displayGraph: DirectedGraphData[Vertex, VertexInfo, EdgeInfo] = DirectedGraphData[Vertex, VertexInfo, EdgeInfo](Set.empty, Set.empty, Map.empty, Map.empty),
  dimensions: Vec2 = Vec2(100, 100),
  graphConfig: GraphConfig = GraphConfig(),
  simConfig: SimulationConfig = SimulationConfig(),
  visConfig: VisualizationConfig = VisualizationConfig(),
  sliderWidget: Boolean = false
) {
}

case class HoverVertex(v: graph.Vertex) extends Action
case object UnHoverVertex extends Action
case class SelectVertex(v: graph.Vertex) extends Action
case class DeselectVertex(v: graph.Vertex) extends Action
case class SetDisplayGraph(graph: DirectedGraphData[Vertex, VertexInfo, EdgeInfo]) extends Action
case class SetDimensions(dimensions: Vec2) extends Action
case class DownloadPublications(url: Iterable[String]) extends Action
case class SetPublications(ps: Seq[Publication]) extends Action
case class SetAvailableIkzList(availableIkzs: Seq[String]) extends Action
case class SetIkz(selectedIkzs: Set[String]) extends Action
case class SetConfig(config: Config) extends Action
case class ShowSliderWidget(show: Boolean) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization())

  val publicaitonsHandler = new ActionHandler(zoomRW(_.publicationVisualization)((m, v) => m.copy(publicationVisualization = v))) {
    override def handle = {
      case SetDisplayGraph(g) =>
        console.log(s"displaying graph with ${g.vertices.size} vertices and ${g.edges.size} edges.")
        updated(value.copy(displayGraph = g))
      case SetPublications(ps) => updated(
        value.copy(publications = ps),
        Effect.action(SetDisplayGraph(tigrs.graph.mergedGraph(value.graphConfig.pubSimilarity, value.graphConfig.authorSimilarity)(ps)))
      )
      case SetAvailableIkzList(availableIkzs) => updated(value.copy(availableIkzs = availableIkzs))
      case SetIkz(selectedIkzs) =>
        updated(
          value.copy(selectedIkzs = selectedIkzs),
          Effect.action(DownloadPublications(selectedIkzs.map(ikz => s"fakall.ikz.${ikz}")))
        )
      case SetDimensions(dim) => updated(value.copy(dimensions = dim))
      case DownloadPublications(urls) =>
        val pubsF: Future[Iterable[Seq[Publication]]] = Future.sequence(urls.map(Data.downloadPublications))
        effectOnly(Effect(
          pubsF.map(pubs => SetPublications(pubs.reduceOption(_ ++ _).getOrElse(Nil).distinct))
        ))
      case SetConfig(c: GraphConfig) => updated(value.copy(graphConfig = c))
      case SetConfig(c: SimulationConfig) => updated(value.copy(simConfig = c))
      case SetConfig(c: VisualizationConfig) => updated(value.copy(visConfig = c))
      case ShowSliderWidget(show) => updated(value.copy(sliderWidget = show))
    }
  }
  val hoverHandler = new ActionHandler(zoomRW(m => m)((m, v) => v)) {
    override def handle = {
      case HoverVertex(v) => {
        if (value.selectedVertices contains v)
          updated(value.copy(hoveredVertex = None))
        else
          updated(value.copy(hoveredVertex = Some(v)))
      }
      case UnHoverVertex => updated(value.copy(hoveredVertex = None))
    }
  }
  val selectionHandler = new ActionHandler(zoomRW(m => m.selectedVertices)((m, v) => m.copy(selectedVertices = v))) {
    override def handle = {
      case SelectVertex(v) => updated(value :+ v, Effect.action(UnHoverVertex))
      case DeselectVertex(v) => updated(value diff Vector(v))
    }
  }
  val actionHandler = composeHandlers(publicaitonsHandler, hoverHandler, selectionHandler)
}
