package tutorial.webapp

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js.Dynamic.global

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

case class RootModel(data: List[String])
case class Add(item: String)
case class Remove(item: String)

object AppCircuit extends Circuit[RootModel] with ReactConnector[RootModel] {
  def initialModel = RootModel(List("Init"))
  def actionHandler: HandlerFunction = (model, action) => action match {
    case Add(item) => Some(ModelUpdate(model.copy(data = item :: model.data)))
    case Remove(item) => Some(ModelUpdate(model.copy(data = model.data diff List(item))))
    case _ => None
  }
}

object ListView {
  case class Props(proxy: ModelProxy[List[String]])
  case class State(text: String)
  class Backend($: BackendScope[Props, State]) {
    def handleClick(dispatch: AnyRef => Callback) = {
      $.state >>= (state => dispatch(Add(state.text)) >> $.modState(_.copy(text = "")))
    }

    def changeField(e: ReactEventI) = {
      val newValue = e.target.value
      $.modState(_.copy(text = newValue))
    }

    def remove(item: String, dispatch: AnyRef => Callback) = {
      dispatch(Remove(item))
    }

    def render(p: Props, s: State) =
      <.div(
        <.input(^.onChange ==> changeField, ^.value := s.text),
        <.button(^.onClick --> handleClick(p.proxy.dispatch), "boom"),
        <.ul(p.proxy.value.map(text =>
          <.li(^.onClick --> remove(text, p.proxy.dispatch), text)))
      )
  }
  private val smartComponent = ReactComponentB[Props]("SmartComponent")
    .initialState(State(""))
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[List[String]]) = smartComponent(Props(proxy))
}

object Main extends JSApp {
  def main() {
    val sc = AppCircuit.connect(_.data)(ListView(_))
    ReactDOM.render(sc, document.getElementById("container"))
  }
}
