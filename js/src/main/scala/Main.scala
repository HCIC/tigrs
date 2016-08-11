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

import boopickle.Default._

import js.JSConverters._
import scalajs.js.typedarray._
import concurrent.Future
import java.nio.ByteBuffer

import dom.ext.Ajax

object Main extends JSApp {
  import scala.concurrent.ExecutionContext.Implicits.global
  def main() {
    println(s"downloading publication data...")

    import scala.concurrent.ExecutionContext.Implicits.global
    AjaxGetByteBuffer("data/fakall.boo").onSuccess {
      case byteBuffer =>
        import PublicationPickler._
        val publications = Unpickle[Publications].fromBytes(byteBuffer)
        println(s"parsing...")
        println(s"loaded ${publications.publications.size} publications.")
        AppCircuit.dispatch(SetPublications(publications))
    }

    val modelConnect = AppCircuit.connect(m => m)
    ReactDOM.render(modelConnect(mainView(_)), document.getElementById("container"))
  }

  def renderFilters(proxy: ModelProxy[RootModel]) = {
    val model = proxy.value
    val filters = model.publicationVisualization.filters
    def update(filters: (String) => Filters)(e: ReactEventI) = {
      proxy.dispatch(SetFilters(filters(e.target.value)))
    }

    <.div(
      filters.filters.map {
        case f: KeywordFilter =>
          <.div("Keyword:", <.input(^.`type` := "text", ^.value := f.query,
            ^.onChange ==> update(v => filters.copy(keyword = KeywordFilter(v)))))
        case f: AuthorFilter =>
          <.div("Author:", <.input(^.`type` := "text", ^.value := f.query,
            ^.onChange ==> update(v => filters.copy(author = AuthorFilter(v)))))
        case f: LimitFilter =>
          <.div("Limit:", <.input(^.`type` := "number", ^.value := f.limit,
            ^.onChange ==> update(v => filters.copy(limit = LimitFilter(v.toInt.abs)))))
        case f => <.div(f.toString)
      }
    )
  }

  val mainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P { proxy =>
      <.div(
        renderFilters(proxy),
        <.div(
          ^.display := "flex",
          ^.flex := "1 1 auto",
          proxy.wrap(m => m.publicationVisualization.graph)(g => GraphView(g.value, 500, 500)),
          proxy.wrap(m => m.hoveredVertex)(vertexView(_))
        )
      )
    }
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
                  <.ul(keywords.map(k => <.li(k.keyword))),
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
              case k: Keyword =>
                <.div(
                  <.h3(k.keyword)
                )
              case p: Project =>
                <.div(
                  <.h3(p.name),
                  <.h2(p.id)
                )
            }
          case None => ""

        }
      ))
    .build

  def AjaxGetByteBuffer(url: String): Future[ByteBuffer] = {
    Ajax.get(
      url,
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(xhr => TypedArrayBuffer.wrap(xhr.response.asInstanceOf[ArrayBuffer]))
  }

}
