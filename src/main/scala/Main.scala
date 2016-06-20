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

import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.Link

import js.JSConverters._

case class Vec2(x: Double, y: Double) {
  def +(that: Vec2) = Vec2(x + that.x, y + that.y)
  def -(that: Vec2) = Vec2(x - that.x, y - that.y)
  def *(that: Vec2) = Vec2(x * that.x, y * that.y)
  def /(that: Vec2) = Vec2(x / that.x, y / that.y)

  def +(s: Double) = Vec2(x + s, y + s)
  def -(s: Double) = Vec2(x - s, y - s)
  def *(s: Double) = Vec2(x * s, y * s)
  def /(s: Double) = Vec2(x / s, y / s)

  def unary_- = Vec2(-x, -y)

  def lengthSq = x * x + y * y
  def length = Math.sqrt(lengthSq)
  def distanceSq(that: Vec2) = (this.x - that.x) * (this.x - that.x) + (this.y - that.y) * (this.y - that.y)
  def distance(that: Vec2) = Math.sqrt(distanceSq(that))
  def normalized = this / length
}

case class Vertex(r: Double) {
  override def toString = s"V(${r.toInt})"
}
case class Edge(source: Vertex, target: Vertex)
case class Graph(vertices: Set[Vertex] = Set.empty, edges: Set[Edge] = Set.empty) {
  def +(v: Vertex) = copy(vertices = vertices + v)
  def +(e: Edge) = copy(edges = edges + e)
  def -(v: Vertex) = copy(vertices = vertices - v, edges = edges.filterNot(e => e.source == v || e.target == v))
}

case class RootModel(graph: Graph)
case class AddVertex(v: Vertex)
case class AddEdge(e: Edge)
case class RemoveVertex(v: Vertex)

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = {
    val vertexCount = 5
    val edgeCount = 3
    def ri(x: Int) = scala.util.Random.nextInt(x)
    def rd = scala.util.Random.nextDouble
    val vertices = Array.tabulate(vertexCount)(i => Vertex(5 + rd * 10))
    def randomVertices(n: Int) = scala.util.Random.shuffle(vertices.toSeq).take(n)
    val edges = Array.tabulate(edgeCount)(i => { val vs = randomVertices(2); Edge(vs(0), vs(1)) })
    RootModel(Graph(vertices.toSet, edges.toSet))
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

object MyStyles extends StyleSheet.Inline {
  import dsl._

  // val vStyle = styleF(Domain.ofRange(0 to 200)) {
  //   case (x) => styleS(
  //     position.absolute,
  //     left(15 px)
  //   // top(y px)
  //   )
  // }
}

object GraphView {
  // http://bl.ocks.org/sxywu/fcef0e6dac231ef2e54b
  case class Props(proxy: ModelProxy[Graph]) {
    def graph = proxy.value
  }
  case class State()
  class Backend($: BackendScope[Props, State]) {
    val width = 960.0
    val height = 500.0
    val charge = -0.3

    val vertexGroupRef = Ref[dom.Element]("vertices")
    val edgeGroupRef = Ref[dom.Element]("edges")

    var vertices: js.Array[D3Vertex] = js.Array()
    var edges: js.Array[D3Edge] = js.Array()

    val force = d3.layout.force[D3Vertex, D3Edge]()
      .charge((d: D3Vertex, _: Double) => d.v.r * d.v.r * charge)
      .linkDistance(100)
      .size((width, height))

    def updateGraph(nextProps: Props) {
      val oldVertices = vertices.map(d => d.v -> d).toMap
      vertices = nextProps.graph.vertices.map { v =>
        oldVertices.get(v) match {
          case Some(d3v) => d3v
          case None => new D3Vertex(v)
        }
      }.toJSArray

      val vertexMap = vertices.map(d => d.v -> d).toMap
      val oldEdges = edges.map(d => (d.e -> d)).toMap
      edges = nextProps.graph.edges.map { e =>
        oldEdges.get(e) match {
          case Some(d3e) => d3e
          case None => new D3Edge(e, vertexMap(e.source), vertexMap(e.target))
        }
      }.toJSArray

      for (vertexGroup <- vertexGroupRef($); edgeGroup <- edgeGroupRef($)) {

        val domVertices = d3.select(vertexGroup).selectAll("circle")
          .data(vertices)
        domVertices.exit().remove()
        domVertices.enter().append("circle")
        domVertices
          .attr("cx", (d: D3Vertex) => d.x)
          .attr("cy", (d: D3Vertex) => d.y)
          .attr("r", (d: D3Vertex) => d.v.r)
          .style("fill", "steelblue")
          .on("click", (d: D3Vertex) => nextProps.proxy.dispatch(RemoveVertex(d.v)).runNow())

        val domEdges = d3.select(edgeGroup).selectAll("line")
          .data(edges)
        domEdges.exit().remove()
        domEdges.enter().append("line")
        domEdges
          .attr("x1", (d: D3Edge) => d.source.x)
          .attr("y1", (d: D3Edge) => d.source.y)
          .attr("x2", (d: D3Edge) => d.target.x)
          .attr("y2", (d: D3Edge) => d.target.y)
          .style("stroke", "#666")
          .style("stroke-width", 2)

        force.nodes(vertices).links(edges)
        force.start()
      }
    }

    def registerTick: Callback = {
      $.state.map { state =>
        for (vertexGroup <- vertexGroupRef($); edgeGroup <- edgeGroupRef($)) {
          force.on("tick", (e: Event) => {
            val domVertices = d3.select(vertexGroup).selectAll("circle")
            val domEdges = d3.select(edgeGroup).selectAll("line")
            domVertices
              .attr("cx", (d: D3Vertex) => d.x)
              .attr("cy", (d: D3Vertex) => d.y)
            domEdges
              .attr("x1", (d: D3Edge) => d.source.x)
              .attr("y1", (d: D3Edge) => d.source.y)
              .attr("x2", (d: D3Edge) => d.target.x)
              .attr("y2", (d: D3Edge) => d.target.y)
            ()
          })
        }
      }.void
    }

    def stopForce = Callback {
      force.stop()
    }

    def render(p: Props, s: State) = {
      <.div(
        <.button(^.onClick --> p.proxy.dispatch(AddVertex(Vertex(r = scala.util.Random.nextDouble * 20 + 10))), "add vertex"),
        <.button(^.onClick --> {
          val vs = scala.util.Random.shuffle(p.graph.vertices.toSeq).take(2)
          p.proxy.dispatch(AddEdge(Edge(vs(0), vs(1))))
        }, "add edge"),
        <.div(
          ^.width := "1000px",
          ^.height := "1000px",
          ^.position := "relative",
          <.svg.svg(
            ^.width := "1000px",
            ^.height := "1000px",
            ^.position := "absolute",
            ^.ref := "edges"
          ),
          <.svg.svg(
            ^.width := "1000px",
            ^.height := "1000px",
            ^.position := "absolute",
            ^.ref := "vertices"
          )
        )
      )
    }
  }
  private val component = ReactComponentB[Props]("SmartComponent")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => Callback { c.backend.updateGraph(c.props) } >> c.backend.registerTick)
    .shouldComponentUpdate(c => { c.$.backend.updateGraph(c.currentProps); false }) // let d3 update, instead of react
    .componentWillUnmount(_.backend.stopForce)
    .build

  def apply(proxy: ModelProxy[Graph]) = component(Props(proxy))
}

@JSExport
class D3Vertex(
  val v: Vertex,
  @(JSExport @field) var x: js.UndefOr[Double] = js.undefined,
  @(JSExport @field) var y: js.UndefOr[Double] = js.undefined
)

@JSExport
class D3Edge(
  val e: Edge,
  @(JSExport @field) var source: D3Vertex,
  @(JSExport @field) var target: D3Vertex
) extends Link[D3Vertex]

object Main extends JSApp {
  def main() {
    // MyStyles.addToDocument()
    val sc = AppCircuit.connect(_.graph)(GraphView(_))
    ReactDOM.render(sc, document.getElementById("container"))
  }
}
