package tigrs

import collection.mutable

import scala.scalajs.js.JSApp
import org.scalajs.dom._
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js.Dynamic.global

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.Defaults._
import scalacss.ScalaCssReact._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

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
case class Graph(vertices: Array[Int] = Array.empty, edges: Array[Edge] = Array.empty)
// case class ForceSimulation(graph: Graph, running: Boolean = false, now: Double = System.currentTimeMillis) {
//   def updated(newNow: Double): ForceSimulation = {
//     val forces = mutable.HashMap.empty[Vertex, Vec2]
//     val delta = newNow - now
//     val newVertices = graph.vertices.map { vertex =>
//       val wantedDist = 200
//       val currentVecs = (graph.vertices - vertex).map(_.pos - vertex.pos)
//       val forces = currentVecs.map(v => v.normalized * (v.length - wantedDist) * 0.01)
//       val otherVertexForces = forces.reduce(_ + _)

//       val gravityForce = -(vertex.pos / Math.pow((vertex.pos.length + 1), 0.5))

//       vertex.copy(pos = vertex.pos + otherVertexForces + gravityForce)
//       // vertex.copy(pos = vertex.pos + 1)
//     }
//     copy(graph = graph.copy(vertices = newVertices), now = newNow)
//   }
// }

case class RootModel(graph: Graph)
case object Update extends RAFAction
case object ToggleRunning extends RAFAction

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = {
    val vertexCount = 10
    val edgeCount = 20
    RootModel(Graph(
      Array.tabulate(vertexCount)(i => i), Array.tabulate(edgeCount)(i => Edge((i * 229384) % vertexCount, (i * 320391) % vertexCount))
    ))
  }
  // zoom into the model, providing access only to the animations
  // val animationHandler = new AnimationHandler(zoomRW(_.simulation)((m, v) => m.copy(simulation = v)))

  // val timestampHandler = new ActionHandler(zoomRW(_.simulation.now)((m, v) => m.copy(m.simulation.copy(now = v)))) {
  //   override def handle = {
  //     case RAFTimeStamp(time) =>
  //       updated(time)
  //   }
  // }

  val actionHandler: HandlerFunction = { case _ => None }
}

// class AnimationHandler[M](modelRW: ModelRW[M, ForceSimulation]) extends ActionHandler(modelRW) {
//   import scala.concurrent.ExecutionContext.Implicits.global
//   override def handle = {
//     case Update =>
//       if (value.running) {
//         updated(value.updated(System.currentTimeMillis), Effect.action(Update))
//       } else {
//         updated(value.updated(System.currentTimeMillis))
//       }
//     case ToggleRunning =>
//       updated(value.copy(running = !value.running), Effect.action(Update))
//   }
// }

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

object SimulationView {
  case class Props(proxy: ModelProxy[Graph])
  case class State(vertexPositions: Map[Int, Vec2], running: Boolean)
  class Backend($: BackendScope[Props, State]) {
    import MyStyles._

    val wantedDist = 1000
    val wantedEdgeDist = 200
    val gravityCenter = Vec2(200, 200)

    def simulationStep(old: State, g: Graph): State = {
      var newPositions = old.vertexPositions
      newPositions = old.vertexPositions.map {
        case (v, pos) =>
          var newPos = pos
          for (other <- g.vertices if other != v) {
            val vec = old.vertexPositions(other) - pos
            val force = vec.normalized * (vec.length - wantedDist) * 0.001
            newPos += force
          }
          val gravityVec = pos - gravityCenter
          val gravityForce = -(gravityVec / Math.pow((gravityVec.length + 1), 0.5))
          newPos += gravityForce

          (v -> newPos)
      }
      g.edges.foreach {
        case Edge(in, out) =>
          val inPos = old.vertexPositions(in)
          val outPos = old.vertexPositions(out)
          val vec = inPos - outPos
          if (vec.length > 0) {
            val force = vec.normalized * (vec.length - wantedEdgeDist) * 0.1
            newPositions = newPositions.updated(in, newPositions(in) - force)
            newPositions = newPositions.updated(out, newPositions(out) + force)
          }
      }
      old.copy(vertexPositions = newPositions)
    }

    val tick: Callback = {
      $.modState(s => {
        // println("tick")
        simulationStep(old = s, g = $.props.runNow().proxy.value)
        // s.copy(vertexPositions = s.vertexPositions.mapValues(_ + 1))
      }) >>
        $.state.map((s: State) => { if (s.running) nextFrame() })
    }

    def nextFrame() {
      window.requestAnimationFrame((_: Double) => tick.runNow())
    }

    def start = Callback { println("start") } >> $.modState(s => s.copy(running = true)) >> Callback { nextFrame() }
    def stop = Callback { println("stop") } >> $.modState(s => s.copy(running = false))
    def toggle = $.state.flatMap { s => if (s.running) stop else start }
    def reset = $.setState($.props.map(init).runNow())

    def render(p: Props, s: State) = {
      <.div(
        <.button(^.onClick --> toggle, if (s.running) "stop" else "start"),
        <.button(^.onClick --> reset, "reset"),
        <.div(
          <.svg.svg(
            ^.width := "500",
            ^.height := "500",
            ^.position := "absolute",
            ^.top := "30px",
            ^.left := "10px",
            // ^.transform := "translate(50%, 50%)", ^.width := "100%", ^.height := "100%",
            // ^.background := "rgba(240, 240, 240, 240)",
            p.proxy.value.edges.map { e =>
              <.svg.line(
                ^.svg.x1 := s.vertexPositions(e.in).x,
                ^.svg.y1 := s.vertexPositions(e.in).y,
                ^.svg.x2 := s.vertexPositions(e.out).x,
                ^.svg.y2 := s.vertexPositions(e.out).y,
                ^.svg.stroke := "#666",
                ^.svg.strokeWidth := "3"
              )
            }
          ),
          // ^.background := "#EFEFEF",
          <.div(
            ^.width := "500px",
            ^.height := "500px",
            ^.position := "absolute",
            ^.top := "30px",
            ^.left := "10px",
            // ^.transform := "translate(50%, 50%)", ^.width := "100%", ^.height := "100%",
            // ^.background := "rgba(240, 240, 240, 240)",
            p.proxy.value.vertices.map { v =>
              val pos = s.vertexPositions(v)
              <.div(^.position := "absolute", ^.left := s"${pos.x}px", ^.top := s"${pos.y}px", ^.border := "5px solid brown", ^.borderRadius := "50%")
            }
          )
        )
      )
    }
  }
  def init(p: Props) = State(p.proxy.value.vertices.zipWithIndex.map { case (v, i) => v -> Vec2(i, (i * i) * 0.01) }.toMap, running = false)
  private val smartComponent = ReactComponentB[Props]("SmartComponent")
    .initialState_P(init)
    .renderBackend[Backend]
    // .componentDidMount(_.backend.start)
    // .componentWillUnmount(_.backend.clear)
    .build

  def apply(proxy: ModelProxy[Graph]) = smartComponent(Props(proxy))
}

object Main extends JSApp {
  def main() {
    MyStyles.addToDocument()
    val sc = AppCircuit.connect(_.graph)(SimulationView(_))
    // AppCircuit.addProcessor(new RAFBatcher)
    // AppCircuit.dispatch(Update)
    ReactDOM.render(sc, document.getElementById("container"))
  }
}
