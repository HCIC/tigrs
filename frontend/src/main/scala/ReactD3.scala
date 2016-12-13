package fdietze.scalajs.react.components

import collection.mutable

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.console
import org.scalajs.dom.raw
import js.JSConverters._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

abstract class D3[_Props](componentName: String = "D3") {
  type Props = _Props
  type Scope = BackendScope[Props, Unit]

  abstract class D3Backend($: Scope) {
    def render(p: Props) = <.div(^.ref := "component")
    lazy val component = Ref[raw.HTMLElement]("component")($).get

    def init(p: Props) = Callback.empty
    def update(p: Props, oldProps: Option[Props] = None) = Callback.empty
    def cleanup() = Callback.empty
  }
  val backendFactory: Scope => D3Backend

  protected val component = ReactComponentB[Props](componentName)
    .backend(backendFactory(_))
    .render(c => c.backend.render(c.props))
    .componentDidMount(c => c.backend.init(c.props) >> c.backend.update(c.props, None))
    .componentWillReceiveProps(c => c.$.backend.update(c.nextProps, Some(c.currentProps)))
    .shouldComponentUpdate(_ => false) // let d3 handle the update, instead of react
    .componentWillUnmount(c => c.backend.cleanup())
    .build

  def apply(p: Props) = component(p)
}

// object D3Demo extends D3[List[Int]]("D3Demo") {
//   class Backend($: Scope) extends D3Backend($) {

//     override def update(p: Props) = Callback {
//       val b = component.selectAll("b")
//         .data(p.toJSArray)

//       b
//         .attr("class", "update")

//       b.enter()
//         .append("b")
//         .text((d: Int) => d.toString)

//       b
//         .text((d: Int) => d.toString)

//       b.exit()
//         .remove()
//     }
//   }

//   val backendFactory = new Backend(_)
// }
