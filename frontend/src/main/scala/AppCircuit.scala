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
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import cats.syntax.either._

import graph._

case class RootModel(
  publicationVisualization: PublicationVisualization,
  // hoveredVertex: Option[graph.Vertex] = None,
  selectedVertices: Vector[graph.Vertex] = Vector.empty
)

case class Search(title: String = "") {
  def isEmpty = title.isEmpty
}

sealed trait Config

case class GraphConfig(
  pubSimilarity: Double = 1.0,
  authorSimilarity: Double = 1.0,
  fractionalCounting: Boolean = UrlConfig().fractionalCounting
) extends Config

case class SimulationConfig(
  repel: Double = 30,
  gravity: Double = 0.03,
  linkDistance: Double = 30
) extends Config

case class VisualizationConfig(
  minYear: Double = 0,
  maxYear: Double = 0,
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
  selectedIkzs: Seq[String] = Nil,
  publications: Seq[Publication] = Nil,
  publicationsMinYear: Double = 0,
  publicationsMaxYear: Double = 0,
  displayGraph: DirectedGraphData[Vertex, VertexInfo, EdgeInfo] = DirectedGraphData[Vertex, VertexInfo, EdgeInfo](Set.empty, Set.empty, Map.empty, Map.empty),
  dimensions: Vec2 = Vec2(100, 100),
  graphConfig: GraphConfig = GraphConfig(),
  simConfig: SimulationConfig = SimulationConfig(),
  visConfig: VisualizationConfig = VisualizationConfig(),
  showSettings: Boolean = true,
  showIkzSelector: Boolean = true
) {
}

case class UrlConfig(
  ikz: List[String] = List("080025"),
  fractionalCounting: Boolean = false,
  showSettings: Boolean = true,
  showIkzSelector: Boolean = true
)

// case class HoverVertex(v: graph.Vertex) extends Action
// case object UnHoverVertex extends Action
case class SelectVertex(v: graph.Vertex) extends Action
case class DeselectVertex(v: graph.Vertex) extends Action
case object ClearSelectedVertices extends Action
case class SetDisplayGraph(graph: DirectedGraphData[Vertex, VertexInfo, EdgeInfo]) extends Action
case class SetDimensions(dimensions: Vec2) extends Action
case class DownloadPublications(url: Iterable[String]) extends Action
case class SetPublications(ps: Seq[Publication]) extends Action
case class SetAvailableIkzList(availableIkzs: Seq[String]) extends Action
case class SetIkz(selectedIkzs: Seq[String]) extends Action
case class SetConfig(config: Config) extends Action
case class ShowSettings(show: Boolean) extends Action
case class ShowIkzSelector(show: Boolean) extends Action
case object ExportHash extends Action
case object ImportHash extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization())

  window.addEventListener("hashchange", (e: Event) => dispatch(ImportHash))

  val globalHandler = new ActionHandler(zoomRW(m => m)((m, v) => v)) {
    import js.Dynamic.{global => g}
    override def handle = {
      case ImportHash =>
        println("importing from hash...")
        val encoded = g.decodeURIComponent(window.location.hash.tail).asInstanceOf[String]
        decode[UrlConfig](encoded).map { newConfig =>
          println("importing from hash successful.")
          val vis = value.publicationVisualization
          val graphConfig = vis.graphConfig
          effectOnly(Effect.action(
            ActionBatch((
              (if (newConfig.ikz.toSet != vis.selectedIkzs.toSet) Seq(SetIkz(newConfig.ikz)) else Nil) ++
              (if (newConfig.fractionalCounting != graphConfig.fractionalCounting) Seq(SetConfig(graphConfig.copy(fractionalCounting = newConfig.fractionalCounting))) else Nil) ++
              Seq(ShowSettings(newConfig.showSettings)) ++
              Seq(ShowIkzSelector(newConfig.showIkzSelector))
            ): _*)
          ))
        } getOrElse {
          console.log("parsing hash failed:", encoded)
          ActionResult.NoChange
        }

      case ExportHash =>
        println("exporting to hash...")
        val vis = value.publicationVisualization

        val encoded = UrlConfig(
          ikz = value.publicationVisualization.selectedIkzs.toList,
          fractionalCounting = value.publicationVisualization.graphConfig.fractionalCounting,
          showSettings = vis.showSettings,
          showIkzSelector = vis.showIkzSelector
        ).asJson.noSpaces
        val hash = s"#${g.encodeURIComponent(encoded).asInstanceOf[String]}"

        // history.pushState
        // this does not trigger the event "hashchang"
        // and as a nice byproduct it makes the back-button an undo-button.
        window.history.pushState(null, null, hash)
        // window.location.hash = hash

        ActionResult.NoChange
    }
  }
  val publicaitonsHandler = new ActionHandler(zoomTo(_.publicationVisualization)) {
    override def handle = {
      case SetDisplayGraph(g) =>
        console.log(s"displaying graph with ${g.vertices.size} vertices and ${g.edges.size} edges.")
        updated(
          value.copy(displayGraph = g),
          Effect.action(ClearSelectedVertices)
        )
      case SetPublications(ps) =>
        val minYear = if (ps.isEmpty) 0 else ps.minBy(_.origin.year).origin.year
        val maxYear = if (ps.isEmpty) 0 else ps.maxBy(_.origin.year).origin.year
        updated(
          value.copy(
            publications = ps,
            publicationsMinYear = minYear,
            publicationsMaxYear = maxYear,
            visConfig = value.visConfig.copy(
              minYear = minYear,
              maxYear = maxYear
            )
          ),
          Effect.action(SetDisplayGraph(tigrs.graph.mergedGraph(value.graphConfig.pubSimilarity, value.graphConfig.authorSimilarity, value.graphConfig.fractionalCounting)(ps)))
        )
      case SetAvailableIkzList(availableIkzs) => updated(value.copy(availableIkzs = availableIkzs))
      case SetIkz(selectedIkzs) =>

        val newIkzs = selectedIkzs diff value.selectedIkzs
        val ga = js.Dynamic.global.ga
        newIkzs.foreach(ikz => ga("send", "event", "ikz", ikz))

        updated(
          value.copy(selectedIkzs = selectedIkzs),
          Effect.action(ActionBatch(
            ExportHash,
            DownloadPublications(selectedIkzs.map(ikz => s"fakall.ikz.${ikz}"))
          ))
        )
      case SetDimensions(dim) => updated(value.copy(dimensions = dim))
      case DownloadPublications(urls) =>
        val pubsF: Future[Iterable[Seq[Publication]]] = Future.sequence(urls.map(Data.downloadPublications))
        effectOnly(Effect(
          pubsF.map(pubs => SetPublications(pubs.reduceOption(_ ++ _).getOrElse(Nil).distinct))
        ))
      case SetConfig(c: GraphConfig) => updated(value.copy(graphConfig = c), Effect.action(ActionBatch((
        Seq(ExportHash) ++
        (if (c != value.graphConfig) Seq(SetDisplayGraph(tigrs.graph.mergedGraph(c.pubSimilarity, c.authorSimilarity, c.fractionalCounting)(value.publications))) else Nil)
      ): _*)))
      case SetConfig(c: SimulationConfig) => updated(value.copy(simConfig = c))
      case SetConfig(c: VisualizationConfig) => updated(value.copy(visConfig = c))
      case ShowSettings(show) => updated(value.copy(showSettings = show))
      case ShowIkzSelector(show) => updated(value.copy(showIkzSelector = show))
    }
  }
  val selectionHandler = new ActionHandler(zoomTo(_.selectedVertices)) {
    override def handle = {
      case SelectVertex(v) => updated(value :+ v) //, Effect.action(UnHoverVertex))
      case DeselectVertex(v) => updated(value diff Vector(v))
      case ClearSelectedVertices => updated(Vector.empty)
    }
  }
  val actionHandler = composeHandlers(globalHandler, publicaitonsHandler, selectionHandler)
}
