package tigrs

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

case class Vertex(id: Int, pos: Vec2)
case class Edge(in: Vertex, out: Vertex)
case class Graph(vertices: Set[Vertex] = Set.empty, edges: Set[Edge] = Set.empty)
case class ForceSimulation(graph: Graph, running: Boolean = false, now: Double = System.currentTimeMillis) {
  def updated(newNow: Double): ForceSimulation = {
    val delta = newNow - now
    val newVertices = graph.vertices.map { vertex =>
      val wantedDist = 100
      val currentVecs = (graph.vertices - vertex).map(_.pos - vertex.pos)
      val forces = currentVecs.map(v => v.normalized * (v.length - wantedDist) * 0.01)
      vertex.copy(pos = vertex.pos + forces.reduce(_ + _))
      // vertex.copy(pos = vertex.pos + 1)
    }
    copy(graph = graph.copy(vertices = newVertices), now = newNow)
  }
}

case class RootModel(simulation: ForceSimulation)
case object Update extends RAFAction
case object ToggleRunning extends RAFAction

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(ForceSimulation(Graph(
    Set(
      Vertex(0, Vec2(0, 0)),
      Vertex(1, Vec2(1, 1)),
      Vertex(2, Vec2(2, 3))
    )
  )))
  // zoom into the model, providing access only to the animations
  val animationHandler = new AnimationHandler(zoomRW(_.simulation)((m, v) => m.copy(simulation = v)))

  val timestampHandler = new ActionHandler(zoomRW(_.simulation.now)((m, v) => m.copy(m.simulation.copy(now = v)))) {
    override def handle = {
      case RAFTimeStamp(time) =>
        updated(time)
    }
  }

  val actionHandler = composeHandlers(animationHandler, timestampHandler)
}

class AnimationHandler[M](modelRW: ModelRW[M, ForceSimulation]) extends ActionHandler(modelRW) {
  import scala.concurrent.ExecutionContext.Implicits.global
  override def handle = {
    case Update =>
      if (value.running) {
        updated(value.updated(System.currentTimeMillis), Effect.action(Update))
      } else {
        updated(value.updated(System.currentTimeMillis))
      }
    case ToggleRunning =>
      updated(value.copy(running = !value.running), Effect.action(Update))
  }
}

object MyStyles extends StyleSheet.Inline {
  import dsl._

  def vertex(x: Double, y: Double) = style(
    position.absolute,
    left(x px),
    top(y px)
  )
}
object SimulationView {
  case class Props(proxy: ModelProxy[ForceSimulation])
  case class State()
  class Backend($: BackendScope[Props, State]) {
    import MyStyles._
    def render(p: Props, s: State) = {
      <.div(
        <.button(^.onClick --> p.proxy.dispatch(ToggleRunning), "start"),
        <.div(
          p.proxy.value.graph.vertices.toSeq.map { v =>
            <.div(vertex(v.pos.x, v.pos.y), v.toString)
          }
        )
      )
    }
  }
  private val smartComponent = ReactComponentB[Props]("SmartComponent")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[ForceSimulation]) = smartComponent(Props(proxy))
}

object Main extends JSApp {
  def main() {
    MyStyles.addToDocument()
    val sc = AppCircuit.connect(_.simulation)(SimulationView(_))
    AppCircuit.addProcessor(new RAFBatcher)
    // AppCircuit.dispatch(Update)
    ReactDOM.render(sc, document.getElementById("container"))
  }
}
