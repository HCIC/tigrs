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
  filters: Seq[Filter] = Nil
) {
  lazy val publicationFilters = filters.collect { case f: PublicationFilter => f }
  lazy val graphFilters = filters.collect { case f: GraphFilter => f }
  lazy val graph: Graph[PubVertex, DiEdge] = {

    println("applying publication filters...")
    val filteredPublications = publicationFilters.foldLeft(publications) { (g, f) => println(f.getClass.getName); f(g) }
    println("constructing graph...")
    val fullGraph = filteredPublications.toGraph
    println("applying graph filters...")
    val g = graphFilters.foldLeft(fullGraph) { (g, f) => println(f.getClass.getName); f(g) }
    println(s"displaying ${g.nodes.size} vertices...")
    g
  }
}

case class SetPublications(publications: Publications) extends Action
case class HoverVertex(v: PubVertex) extends Action
case object UnHoverVertex extends Action
case object UpdateGraph extends Action
case class AddFilter(filter: Filter) extends Action
case class RemoveFilter(index: Int) extends Action
case class UpdateFilter(index: Int, filter: Filter) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(PublicationVisualization(
    filters = (
      PublicationKeywordFilter("Medizin") ::
      PublicationLimitFilter(2000) ::
      Nil
    )
  ))

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
  val filterHandler = new ActionHandler(zoomRW(_.publicationVisualization.filters)((m, v) => m.copy(publicationVisualization = m.publicationVisualization.copy(filters = v)))) {
    override def handle = {
      case AddFilter(f) => updated(f +: value)
      case RemoveFilter(i) => updated(value.take(i) ++ value.drop(i + 1))
      case UpdateFilter(i, f) => updated(value.updated(i, f))
    }
  }
  val actionHandler = composeHandlers(publicaitonsHandler, previewHandler, filterHandler)
}
