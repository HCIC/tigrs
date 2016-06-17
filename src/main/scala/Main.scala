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

case class Edge(in: Int, out: Int)
case class Vertex(r: Double)
case class Graph(vertices: IndexedSeq[Vertex] = IndexedSeq.empty, edges: Seq[Edge] = Seq.empty) {
  def +(v: Vertex) = copy(vertices = vertices :+ v)
}

case class RootModel(graph: Graph)
case class AddVertex(v: Vertex)

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = {
    val vertexCount = 10
    val edgeCount = 30
    def ri(x: Int) = scala.util.Random.nextInt(x)
    def rd = scala.util.Random.nextDouble
    val m = RootModel(Graph(
      Array.tabulate(vertexCount)(i => Vertex(rd * 10)), Array.tabulate(edgeCount)(i => Edge(ri(vertexCount), ri(vertexCount)))
    ))
    // println(m.graph.edges.mkString("\n"))
    m

    // RootModel(Graph(Array(0, 1, 2), Array(Edge(0, 1), Edge(0, 2), Edge(1, 2))))
    // RootModel(Graph(Array(0, 1), Array(Edge(0, 1))))
  }

  val graphHandler = new ActionHandler(zoomRW(_.graph)((m, v) => m.copy(graph = v))) {
    override def handle = {
      case AddVertex(v) => updated(value + v)
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
    val nodeGroupRef = Ref[dom.Element]("nodes")

    val height = 500.0
    val width = 960.0
    val charge = -0.3
    val start = System.currentTimeMillis
    var time = 0L
    var ticks = 0L
    var vertices: js.Array[D3Vertex] = js.Array()

    val force = d3.layout.force()
      .charge((d: D3Vertex, _: Double) => d.v.r * d.v.r * charge)
      .size((width, height))

    def updateGraph(nextProps: Props) {
      val oldVertices = vertices.map(_.v)
      val newVertices = nextProps.graph.vertices

      val addedVertices = newVertices diff oldVertices
      val updatedVertices = (newVertices intersect oldVertices).toSet

      vertices = vertices.filter(d3v => updatedVertices(d3v.v)) ++ addedVertices.map(new D3Vertex(_))

      nodeGroupRef($).map { vertexGroup =>
        val domVertices = d3.select(vertexGroup).selectAll("circle")
          .data(vertices)

        domVertices.enter().append("circle")
          .attr("r", (d: D3Vertex) => d.v.r)
          .style("fill", "steelblue")

        domVertices
          .attr("cx", (d: D3Vertex) => d.x).attr("cy", (d: D3Vertex) => d.y)

        domVertices.exit().remove()

        force.nodes(vertices) //.links(nextProps.links)
        force.start()
      }
    }

    def registerTick: Callback = {
      $.state.map { state =>
        for (vertexGroup <- nodeGroupRef($)) {
          force.on("tick", (e: Event) => {
            val domVertices = d3.select(vertexGroup).selectAll("circle")

            val renderStart = System.currentTimeMillis
            domVertices.attr("cx", (d: D3Vertex) => d.x).attr("cy", (d: D3Vertex) => d.y)

            time += (System.currentTimeMillis - renderStart)
            ticks += 1
          })

          force.on("end", (e: Event) => {
            val totalTime = System.currentTimeMillis - start
            console.log("Total Time:", totalTime)
            console.log("Render Time:", time)
            console.log("Ticks:", ticks)
            console.log("Average Time:", totalTime / ticks)
          })
        }
      }.void
    }

    def stopForce = Callback {
      force.stop()
    }

    def render(p: Props, s: State) = {
      val ref = "refuditaern".reactAttr
      <.div(
        <.button(^.onClick --> p.proxy.dispatch(AddVertex(Vertex(r = scala.util.Random.nextDouble * 30))), "add vertex"),
        <.svg.svg(^.width := "1000px", ^.height := "1000px", <.svg.g(^.ref := "nodes"))
      )
    }
  }
  private val component = ReactComponentB[Props]("SmartComponent")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => Callback { c.backend.updateGraph(c.props) } >> c.backend.registerTick)
    .shouldComponentUpdate(c => { c.$.backend.updateGraph(c.currentProps); false })
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
object Main extends JSApp {
  def main() {
    // MyStyles.addToDocument()
    val sc = AppCircuit.connect(_.graph)(GraphView(_))
    ReactDOM.render(sc, document.getElementById("container"))
  }
}
