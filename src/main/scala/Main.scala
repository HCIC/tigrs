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

    val force = d3.layout.force()
      .charge((d: D3Vertex, _: Double) => d.v.r * d.v.r * charge)
      .size((width, height))

    // val updateGraph = (selection: org.singlespaced.d3js.Selection[org.scalajs.dom.EventTarget]) => {
    //   selection.selectAll(".node")
    //     .call(updateNode)
    //   selection.selectAll(".link")
    //     .call(updateLink)
    // }

    // var updateNode = (selection: org.singlespaced.d3js.Selection[org.scalajs.dom.EventTarget]) => {
    //   selection.attr("transform", (d: D) => "translate(" + d.x + "," + d.y + ")")
    // }

    // var updateLink = (selection: org.singlespaced.d3js.Selection[org.scalajs.dom.EventTarget]) => {
    //   selection.attr("x1", (d: D) => d.source.x)
    //     .attr("y1", (d: D) => d.source.y)
    //     .attr("x2", (d: D) => d.target.x)
    //     .attr("y2", (d: D) => d.target.y)
    // }

    def registerTick(undefOrnodeSelection: js.UndefOr[org.singlespaced.d3js.Selection[D3Vertex]]): Callback = {
      $.state.map { state =>
        println(s"  state: ${state}")

        for (vertexGroup <- nodeGroupRef($); nodeSelection <- undefOrnodeSelection) {
          println(s"  ref: ${vertexGroup}")
          println(s"registerTick")
          force.on("tick", (e: Event) => {

            val renderStart = System.currentTimeMillis
            nodeSelection.attr("cx", (d: D3Vertex) => d.x).attr("cy", (d: D3Vertex) => d.y)

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
      println("force stop")
      force.stop()
    }

    def updateGraph(nextProps: Props) = CallbackTo[js.UndefOr[org.singlespaced.d3js.Selection[D3Vertex]]] {
      nodeGroupRef($).map { vertexGroup =>
        val vertices = nextProps.graph.vertices.map(new D3Vertex(_)).toJSArray
        println(s"  ref: ${vertexGroup}")
        println("force start")
        force.nodes(vertices) //.links(nextProps.links)
        force.start()

        d3.select(vertexGroup).selectAll("circle")
          .data(vertices)
          .enter().append("circle")
          // .exit().remove()
          .attr("r", (d: D3Vertex) => d.v.r)
          .style(
            "fill", "steelblue"
          )

        // var d3Nodes = this.d3Graph.selectAll('.node')
        //   .data(nextProps.nodes, (node) => node.key)
        // d3Nodes.enter().append('g').call(enterNode)
        // d3Nodes.exit().remove()
        // d3Nodes.call(updateNode)

        // var d3Links = this.d3Graph.selectAll('.link')
        //   .data(nextProps.links, (link) => link.key)
        // d3Links.enter().insert('line', '.node').call(enterLink)
        // d3Links.exit().remove()
        // d3Links.call(updateLink)

        // we should actually clone the nodes and links
        // since we're not supposed to directly mutate
        // props passed in from parent, and d3's force function
        // mutates the nodes and links array directly
        // we're bypassing that here for sake of brevity in example
      }
    }

    def render(p: Props, s: State) = {
      val ref = "refuditaern".reactAttr
      <.div(
        <.button(^.onClick --> p.proxy.dispatch(AddVertex(Vertex(r = 30))), "add vertex"),
        <.svg.svg(^.width := "1000px", ^.height := "1000px", <.svg.g(^.ref := "nodes"))
      )
    }
  }
  private val component = ReactComponentB[Props]("SmartComponent")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.updateGraph(c.props) >>= c.backend.registerTick)
    .shouldComponentUpdate(c => { c.$.backend.updateGraph(c.currentProps).runNow(); false })
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
