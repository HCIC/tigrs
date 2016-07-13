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

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import js.JSConverters._

object Global {
  val faculties = (
    "fak00" ::
    "fak01a" ::
    "fak01b" ::
    "fak01c" ::
    "fak01d" ::
    "fak01e" ::
    "fak01f" ::
    "fak02" ::
    "fak03" ::
    "fak04" ::
    "fak05" ::
    "fak06" ::
    "fak07" ::
    "fak08" ::
    "fak10" ::
    Nil
  )
  val publicationLimit = 10
}

object Main extends JSApp {
  def main() {
    AppCircuit.dispatch(SetFaculty("fak00"))
    val modelConnect = AppCircuit.connect(m => m)
    ReactDOM.render(modelConnect(mainView(_)), document.getElementById("container"))
  }

  val mainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P(proxy =>
      <.div(
        <.div(
          <.select(
            ^.onChange ==> ((e: ReactEventI) => proxy.dispatch(SetFaculty(e.target.value))),
            Global.faculties.map(f => <.option(^.value := f, f))
          )
        ),
        <.div(
          ^.display := "flex",
          ^.flex := "1 1 auto",
          proxy.wrap(m => m.graph)(g => GraphView(g.value, 500, 500)),
          proxy.wrap(m => m.hoveredVertex)(vertexView(_))
        )
      ))
    .build

  val vertexView = ReactComponentB[ModelProxy[Option[PubVertex]]]("PublicationView")
    .render_P(proxy =>
      <.div(
        ^.width := "400px",
        proxy.value match {
          case Some(v) =>
            v match {
              case Publication(title, authors, keywords, outlet, origin, uri, recordId, owner, projects) =>
                <.div(
                  <.h3(title),
                  outlet.map(o => <.div(o.name)),
                  <.ul(authors.map(a => <.li(a.name))),
                  keywords.headOption.map(_ => "Keywords:"),
                  <.ul(keywords.map(k => <.li(k))),
                  <.div(origin.publisher.map(p => s"${p}, "), s"${origin.date}"),
                  uri.map(uri => <.a(^.href := uri, uri)),
                  <.div(s"record: $recordId"),
                  owner.map(_ => "Owner:"),
                  owner.map(institute => <.ul(institute.ikz.map(ikz => <.li(ikz)))),
                  projects.headOption.map(_ => "Projects:"),
                  <.ul(projects.map(p => <.li(p.name)))
                )
              case a: Author =>
                <.div(
                  <.h3(a.name),
                  a.id
                )
              case o: Outlet =>
                <.div(
                  <.h3(o.name)
                )
            }
          case None => ""

        }
      ))
    .build
}
