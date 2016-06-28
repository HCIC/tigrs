package tigrs

import collection.mutable

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom._
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js.Dynamic.global
import scala.annotation.meta.field

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.Defaults._
import scalacss.ScalaCssReact._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import org.singlespaced.d3js
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.Link

import js.JSConverters._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import domPatterns._

case class RootModel(graph: Graph[PubVertex, DiEdge])
case class SetGraph(graph: Graph[PubVertex, DiEdge]) extends Action
case class AddVertex(v: PubVertex) extends Action
case class AddEdge(e: DiEdge[PubVertex]) extends Action
case class RemoveVertex(v: PubVertex) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(Graph.empty)

  val graphHandler = new ActionHandler(zoomRW(_.graph)((m, v) => m.copy(graph = v))) {
    override def handle = {
      case SetGraph(g) => updated(g)
      case AddVertex(v) => updated(value + v)
      case AddEdge(e) => updated(value + e)
      case RemoveVertex(v) => updated(value - v)
    }
  }
  val actionHandler = composeHandlers(graphHandler)
}

trait PubVertex

case class Author(name: String) extends PubVertex
trait Outlet
case class Conference(name: String) extends Outlet
case class Journal(name: String) extends Outlet
case class Origin(date: String, publisher: Option[String])
case class Publication(title: String, authors: Seq[Author], keyWords: Seq[String] = Nil, outlet: Option[Outlet], origin: Origin, uri: Option[String], recordId: String) extends PubVertex

case class Publications(publications: Seq[Publication]) {
  def authors = publications.flatMap(_.authors).distinct
  def toAuthorPublicationGraph: Graph[PubVertex, DiEdge] = {
    val edges = publications.flatMap(p => p.authors.map(a => DiEdge(a, p)))
    val vertices = publications ++ authors
    Graph.from(vertices, edges)
  }
}

object Main extends JSApp {
  def xmlToPublications(tree: Document): Publications = {
    def extractTitle: PartialFunction[Node, String] = { case NodeEx("titleInfo", _, _, Seq(_, NodeEx("title", _, title, _), _)) => title }
    val extractAuthor: PartialFunction[Node, (String, Int)] = {
      case NodeEx("name", Seq(("type", "personal")), _, Seq(_, NodeEx("namePart", _, author, _), _, NodeEx("namePart", _, termsOfAdress, _), _)
        ) =>
        (author, termsOfAdress.toInt)
    }
    def extractKeyWords: PartialFunction[Node, Seq[String]] = { case NodeEx("subject", _, _, Seq(_, NodeEx("topic", _, keyWords, _), _)) => keyWords.split(",").flatMap(_.split(";")).map(_.trim) }
    def extractConference: PartialFunction[Node, Conference] = {
      case NodeEx("name", Seq(("type", "conference")), _, Seq(_, NodeEx("namePart", _, name, _), _*)) =>
        Conference(name)
    }
    def extractJournal: PartialFunction[Node, Journal] = {
      case NodeEx("relatedItem", Seq(("type", "host")), _, Seq(_, NodeEx("titleInfo", _, _, Seq(_, NodeEx("title", _, name, _), _)), _*)) =>
        Journal(name)
    }
    def extractOrigin: PartialFunction[Node, Origin] = {
      case NodeEx("originInfo", _, _, childNodes) =>
        childNodes match {
          case Seq(_, place, _, NodeEx("publisher", _, publisher, _), _, NodeEx("dateIssued", _, date, _), _) => Origin(date, Some(publisher))
          case Seq(_, NodeEx("dateIssued", _, date, _), _) => Origin(date, None)
        }
    }
    def extractUri: PartialFunction[Node, String] = {
      case NodeEx("identifier", Seq(("type", "uri")), uri, _) => uri
    }

    def extractRecordId: PartialFunction[Node, String] = { case NodeEx("recordInfo", _, _, Seq(_, recordChangeDate, _, NodeEx("recordIdentifier", _, recordId, _), _)) => recordId }

    val publications = tree.documentElement.childNodes.collect {
      case mods @ NodeEx("mods", _, _, entries) if entries.collectFirst(extractTitle).isDefined =>
        val title = entries.collectFirst(extractTitle).get
        val authors = entries.collect(extractAuthor).toList.sortBy { case (_, termsOfAdress) => termsOfAdress }.map { case (name, _) => Author(name) }
        val keyWords = entries.collectFirst(extractKeyWords).getOrElse(Nil)
        val outlet = entries.collectFirst(extractConference orElse extractJournal)
        val origin = entries.collectFirst(extractOrigin).get
        val uri = entries.collectFirst(extractUri)
        val recordId = entries.collectFirst(extractRecordId).get
        Publication(title, authors, keyWords, outlet, origin, uri, recordId)
    }.toSeq
    Publications(publications)
  }

  def main() {
    // import fr.hmil.roshttp.HttpRequest
    // import scala.concurrent.ExecutionContext.Implicits.global
    // val request = HttpRequest("http://publications.rwth-aachen.de/search").withQueryStringRaw("ln=de&p=I%3A%28DE-82%29080025_20140620&f=&action_search=Suchen&c=RWTH+Publications&sf=&so=d&rm=&rg=3&sc=0&of=xo")
    // request.send().onSuccess {
    //   case response =>
    //     val xmlData = response.body
    //     val parser = new DOMParser
    //     val tree = parser.parseFromString(xmlData, "text/xml")
    //     val publications = xmlToPublications(tree)
    //     AppCircuit.dispatch(SetGraph(publications.toAuthorPublicationGraph))
    // }
    val parser = new DOMParser
    val tree = parser.parseFromString(ExampleData.xml, "text/xml")
    val publications = xmlToPublications(tree)
    AppCircuit.dispatch(SetGraph(publications.toAuthorPublicationGraph))

    val sc = AppCircuit.connect(m => m)
    ReactDOM.render(sc(mainView(_)), document.getElementById("container"))
  }

  val mainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P(proxy =>
      <.div(
        proxy.wrap(_.graph)(TigrsView(_, 400, 400))
      ))
    .build
}

object TigrsView extends graphView.GraphView[PubVertex, DiEdge] {
  override def styleVertices(sel: VertexSelection) = {
    super.styleVertices(sel)
      .style("fill", (d: D3Vertex) => d.v match {
        case _: Publication => "#4C90EB"
        case _: Author => "#DB6F45"
      })
      .attr("title", (d: D3Vertex) => d.v match {
        case p: Publication => p.title
        case a: Author => a.name
      })
  }
}
