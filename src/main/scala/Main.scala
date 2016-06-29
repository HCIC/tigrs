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

case class Author(name: String, id: String) extends PubVertex
trait Outlet
case class Conference(name: String) extends Outlet
case class Journal(name: String) extends Outlet
case class Series(name: String) extends Outlet
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
    val extractAuthor: PartialFunction[Node, (String, String, Int)] = {
      case NodeEx("name", Seq(("type", "personal")), _, Seq(_,
        NodeEx("namePart", _, author, _), _,
        NodeEx("namePart", _, termsOfAdress, _), _,
        role, _,
        NodeEx("nameIdentifier", _, nameIdentifier, _), _
        )
        ) =>
        (author, nameIdentifier, termsOfAdress.toInt)
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
    def extractSeries: PartialFunction[Node, Series] = {
      case NodeEx("relatedItem", Seq(("type", "series")), _, Seq(_, NodeEx("titleInfo", _, _, Seq(_, NodeEx("title", _, name, _), _)), _*)) =>
        Series(name)
    }
    def extractOrigin: PartialFunction[Node, Origin] = {
      case NodeEx("originInfo", _, _, childNodes) =>
        val date = childNodes.find(_.nodeName == "dateIssued").get.textContent
        val publisher = childNodes.find(_.nodeName == "publisher").map(_.textContent)
        Origin(date, publisher)
    }
    def extractUri: PartialFunction[Node, String] = {
      case NodeEx("identifier", Seq(("type", "uri")), uri, _) => uri
    }

    def extractRecordId: PartialFunction[Node, String] = {
      case NodeEx("recordInfo", _, _, childNodes) =>
        val recordId = childNodes.find(_.nodeName == "recordIdentifier").get.textContent
        recordId
    }

    val publications = tree.documentElement.childNodes.collect {
      case mods @ NodeEx("mods", _, _, entries) if entries.collectFirst(extractTitle).isDefined =>
        try {
          val title = entries.collectFirst(extractTitle).get
          val authors = entries.collect(extractAuthor).toList.sortBy {
            case (_, id, termsOfAdress) => termsOfAdress
          }.map { case (name, id, _) => Author(name, id) }
          val keyWords = entries.collectFirst(extractKeyWords).getOrElse(Nil)
          val outlet = entries.collectFirst(extractConference orElse extractJournal orElse extractSeries)
          val origin = entries.collectFirst(extractOrigin) match {
            case Some(origin) => origin
            case _ => println(mods.toString); Origin("", None)
          }
          val uri = entries.collectFirst(extractUri)
          val recordId = entries.collectFirst(extractRecordId).get
          Publication(title, authors, keyWords, outlet, origin, uri, recordId)
        } catch {
          case e: Exception =>
            console.warn(e.getMessage)
            console.warn("could not parse:", mods.asInstanceOf[js.Any])
            throw e
        }
    }.toSeq
    println(publications.take(10).mkString("\n\n"))
    Publications(publications)
  }

  def main() {

    val xhr = new dom.XMLHttpRequest()
    xhr.open("GET", "data/fak01a.xml")
    xhr.onload = { (e: dom.Event) =>
      if (xhr.status == 200) {
        val tree = xhr.responseXML
        val publications = xmlToPublications(tree)
        AppCircuit.dispatch(SetGraph(publications.toAuthorPublicationGraph))
      }
    }
    xhr.send()

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
