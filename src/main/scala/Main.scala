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

case class Vertex(r: Double) {
  override def toString = s"V(${r.toInt})"
}

case class RootModel(graph: Graph[Vertex, DiEdge])
case class AddVertex(v: Vertex) extends Action
case class AddEdge(e: DiEdge[Vertex]) extends Action
case class RemoveVertex(v: Vertex) extends Action

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = {
    val vertexCount = 5
    val edgeCount = 3
    def rd = scala.util.Random.nextDouble
    val vertices = Array.tabulate(vertexCount)(i => Vertex(5 + rd * 10))
    def randomVertices(n: Int) = scala.util.Random.shuffle(vertices.toSeq).take(n)
    val edges = Array.tabulate(edgeCount)(i => {
      val vs = randomVertices(2);
      DiEdge(vs(0), vs(1))
    })
    RootModel(Graph.from(vertices, edges))
  }

  val graphHandler = new ActionHandler(zoomRW(_.graph)((m, v) => m.copy(graph = v))) {
    override def handle = {
      case AddVertex(v) => updated(value + v)
      case AddEdge(e) => updated(value + e)
      case RemoveVertex(v) => updated(value - v)
    }
  }
  val actionHandler = composeHandlers(graphHandler)
}

trait Outlet
case class Conference(name: String) extends Outlet
case class Journal(name: String) extends Outlet
case class Origin(date: String, publisher: Option[String])
case class Publication(title: String, authors: Seq[String], keyWords: Seq[String] = Nil, outlet: Option[Outlet], origin: Origin, uri: Option[String], recordId: String)

case class Publications(publications: Seq[Publication]) {
  def authors = publications.flatMap(_.authors).distinct
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
        val authors = entries.collect(extractAuthor).toList.sortBy(_._2).map(_._1)
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
    val parser = new DOMParser
    val tree = parser.parseFromString(ExampleData.xml, "text/xml")
    xmlToPublications(tree)

    val sc = AppCircuit.connect(m => m)
    ReactDOM.render(sc(mainView(_)), document.getElementById("container"))
  }

  val mainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P(proxy =>
      <.div(
        <.button(^.onClick --> proxy.dispatch(AddVertex(Vertex(r = scala.util.Random.nextDouble * 20 + 10))), "add vertex"),
        <.button(^.onClick --> {
          val vs = scala.util.Random.shuffle(proxy.value.graph.nodes.toSeq).sortBy(_.degree).take(2)
          proxy.dispatch(AddEdge(DiEdge(vs(0).value, vs(1).value)))
        }, "add edge"),
        proxy.wrap(_.graph)(TigrsView(_, 400, 400)),
        proxy.wrap(_.graph)(ClassicView(_, 200, 200))
      ))
    .build
}

object TigrsView extends graphView.GraphView[Vertex, DiEdge] {
  override def charge(v: Vertex) = v.r * v.r * (-2)
  override def linkDistance(e: DiEdge[Vertex]) = 100
  override def styleVertices(sel: VertexSelection) = {
    super.styleVertices(sel)
      .attr("r", (d: D3Vertex) => d.v.r)
      .on("click", (d: D3Vertex) => AppCircuit.dispatch(RemoveVertex(d.v)))
  }
}

object ClassicView extends graphView.GraphView[Vertex, DiEdge]
