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

import graph._

case class RootModel(
  publicationVisualization: PublicationVisualization,
  hoveredVertex: Option[graph.Vertex] = None
)

case class Search(title: String = "") {
  def isEmpty = title.isEmpty
}

sealed trait Config

case class GraphConfig(
  pubSimilarity: Double = 1.0,
  authorSimilarity: Double = 1.0
) extends Config

case class SimulationConfig(
  repel: Double = 30,
  gravity: Double = 0.03,
  linkDistance: Double = 30
) extends Config

case class VisualizationConfig(
  radiusOffset: Double = 1.0,
  radiusFactor: Double = 1.0,
  radiusExponent: Double = 1.0,
  widthOffset: Double = 1.0,
  widthFactor: Double = 1.0,
  widthExponent: Double = 1.0
) extends Config

case class PublicationVisualization(
  ikzs: Seq[String] = Nil,
  ikz: Option[String] = None,
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
case class SetDisplayGraph(graph: DirectedGraphData[Vertex, VertexInfo, EdgeInfo]) extends Action
case class SetDimensions(dimensions: Vec2) extends Action
case class DownloadPublications(url: String) extends Action
case class SetPublications(ps: Seq[Publication]) extends Action
case class SetIkzList(ikzs: Seq[String]) extends Action
case class SetIkz(ikz: String) extends Action
case class SetConfig(config: Config) extends Action
case class ShowSliderWidget(show: Boolean) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization())

  val publicaitonsHandler = new ActionHandler(zoomRW(_.publicationVisualization)((m, v) => m.copy(publicationVisualization = v))) {
    override def handle = {
      case SetDisplayGraph(g) =>
        console.log(s"displaying graph with ${g.vertices.size} vertices and ${g.edges.size} edges.")
        updated(value.copy(displayGraph = g))
      case SetPublications(ps) => updated(value.copy(publications = ps), Effect(Future { SetDisplayGraph(tigrs.graph.mergedGraph(value.graphConfig.pubSimilarity, value.graphConfig.authorSimilarity)(ps)) }))
      case SetIkzList(ikzs) => updated(value.copy(ikzs = ikzs))
      case SetIkz(ikz) => updated(value.copy(ikz = Some(ikz)), Effect { Future.successful(DownloadPublications(s"fakall.ikz.$ikz")) })
      case SetDimensions(dim) => updated(value.copy(dimensions = dim))
      case DownloadPublications(url) => effectOnly(Effect {
        Data.downloadPublications(url).map {
          graph => (SetPublications(graph))
        }
      })
      case SetConfig(c: GraphConfig) => updated(value.copy(graphConfig = c))
      case SetConfig(c: SimulationConfig) => updated(value.copy(simConfig = c))
      case SetConfig(c: VisualizationConfig) => updated(value.copy(visConfig = c))
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
