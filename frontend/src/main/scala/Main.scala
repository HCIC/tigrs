package tigrs

import collection.mutable

import scala.scalajs.js
import scala.scalajs.js.{JSApp, JSON}
import js.JSConverters._
import org.scalajs.dom
import org.scalajs.dom._
import scala.scalajs.js.annotation._
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.js.Dynamic.global
import scala.annotation.meta.field

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import boopickle.Default._

import pharg._
import vectory._
import shapeless.{Lens, lens}

import js.JSConverters._
import scalajs.js.typedarray._
import concurrent.{Future, Promise, Await}
import concurrent.duration.Duration
import java.nio.ByteBuffer

import org.scalajs.dom.idb.Database
import org.scalajs.dom.raw.IDBVersionChangeEvent

import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

import dom.ext.Ajax

object Data {
  def downloadGraph(name: String): Future[DirectedGraph[graph.Vertex]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    Main.AjaxGetByteBuffer(s"data/$name.boo").map { byteBuffer =>
      println("downloading graph...")
      import PublicationPickler._
      val g = Unpickle[DirectedGraph[graph.Vertex]].fromBytes(byteBuffer)
      println(s"downloaded graph with ${g.vertices.size} vertices and ${g.edges.size} edges.")
      g
    }
  }
}
import Data._

@JSExport
object Visualization {
  import scala.concurrent.ExecutionContext.Implicits.global

  @ScalaJSDefined
  trait WidgetConfig extends js.Object {
    val renderTarget: dom.raw.Element
    val ikz: String
    val sliderWidget: js.UndefOr[Boolean]
  }

  @JSExport
  def render(conf: WidgetConfig) {
    AppCircuit.dispatch(ShowSliderWidget(conf.sliderWidget.getOrElse(false)))

    downloadGraph(s"fakall.ikz.${conf.ikz}.cliquemergedgraph_1.0_1.0").onSuccess {
      case graph =>
        AppCircuit.dispatch(SetGraph(graph))
        updateDimensions
    }
    window.addEventListener("resize", { e: Event => updateDimensions() })

    def updateDimensions() {
      val targetRect = conf.renderTarget.getBoundingClientRect()
      val targetDim = Vec2(targetRect.width, targetRect.height)
      AppCircuit.dispatch(SetDimensions(targetDim))
    }

    val modelConnect = AppCircuit.connect(m => m)
    ReactDOM.render(modelConnect(widgetView(_)), conf.renderTarget)
  }

  val widgetView = ReactComponentB[ModelProxy[RootModel]]("WidgetView")
    .render_P { proxy =>
      <.div(
        ^.width := "100%",
        ^.height := "100%",
        GraphView(
          proxy.value.publicationVisualization.graph,
          proxy.value.publicationVisualization.dimensions,
          Some(GraphConfig(
            proxy.value.publicationVisualization.config,
            proxy.value.hoveredVertex,
            proxy.value.highlightedVertices
          ))
        ),
        proxy.value.publicationVisualization.sliderWidget ?= configWidget(proxy),
        proxy.wrap(m => m.preview)(p => preview(p))
      )
    }.build

  val configWidget = ReactComponentB[ModelProxy[RootModel]]("configWidget")
    .render_P { proxy =>
      val model = proxy.value
      val config = model.publicationVisualization.config

      def configSlider(title: String, min: Double, max: Double, step: Double, lens: Lens[SimulationConfig, Double], additionalDispatch: Option[SimulationConfig => Action] = None) = {
        <.div(
          ^.display := "flex",
          ^.justifyContent := "space-between",
          s"$title: ",
          <.input(
            ^.`type` := "range", ^.min := min, ^.max := max, ^.step := step, ^.value := lens.get(config), ^.title := lens.get(config),
            ^.onChange ==> ((e: ReactEventI) => {
              val newConfig = lens.set(config)(e.target.value.toDouble)
              proxy.dispatchCB(SetConfig(newConfig)) >> {
                additionalDispatch match {
                  case Some(f) => proxy.dispatchCB(f(newConfig))
                  case None => Callback.empty
                }
              }
            })
          )
        )
      }
      <.div(
        ^.position := "absolute",
        ^.top := "30px",
        ^.left := "30px",
        ^.background := "white",
        ^.border := "1px solid #DDD",
        ^.padding := "10px",
        <.div(
          ^.display := "flex",
          ^.flex := "1 1 auto",
          <.div(
            configSlider("Radius", 1, 20, 0.5, lens[SimulationConfig] >> 'radius),
            configSlider("Charge", 1, 1000, 10, lens[SimulationConfig] >> 'charge),
            configSlider("LinkDistance", 1, 100, 1, lens[SimulationConfig] >> 'linkDistance),
            configSlider("LinkStrength", 1, 10, 0.5, lens[SimulationConfig] >> 'linkStrength),
            configSlider("Gravity", 0, 1, 0.01, lens[SimulationConfig] >> 'gravity),
            configSlider("ChargeDistance", 1, 1000, 10, lens[SimulationConfig] >> 'chargeDistance),
            configSlider("PubSimilarity", 0.1, 1.1, 0.1, lens[SimulationConfig] >> 'pubSimilarity,
              Some(c => DownloadGraph(f"fakall.ikz.080013.cliquemergedgraph_${c.pubSimilarity}%.1f_${c.authorSimilarity}%.1f"))),
            configSlider("AuthorSimilarity", 0.1, 1.1, 0.1, lens[SimulationConfig] >> 'authorSimilarity,
              Some(c => DownloadGraph(f"fakall.ikz.080013.cliquemergedgraph_${c.pubSimilarity}%.1f_${c.authorSimilarity}%.1f")))
          )
        )
      )
    }.build

  val preview = ReactComponentB[ModelProxy[Option[AnyRef]]]("PublicationView")
    .render_P(proxy =>
      proxy.value match {
        case Some(data) =>
          <.div(
            ^.position := "absolute",
            ^.top := "30px",
            ^.right := "30px",
            ^.background := "white",
            ^.border := "1px solid #DDD",
            ^.padding := "10px",
            ^.width := "400px",
            data match {
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
              case PublicationSeq(ps) =>
                <.div(
                  <.div(ps.map(p => <.div(s"[${p.origin.date}] ", <.b(p.title)))),
                  <.div(ps.flatMap(p => p.authors).distinct.sortBy(_.name).map(a => <.div(a.name)))
                )
              case AuthorSeq(as) =>
                <.div(
                  <.div(as.map(p => <.div(<.b(p.name))))
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
              case other => other.toString
            }
          )
        case None => <.div()
      })
    .build

}

object Main extends JSApp {

  import scala.concurrent.ExecutionContext.Implicits.global
  def main() {

    downloadGraph("fakall.ikz.080013.cliquemergedgraph_1.0_1.0").onSuccess { case graph => AppCircuit.dispatch(SetGraph(graph)) }

    val modelConnect = AppCircuit.connect(m => m)
    ReactDOM.render(modelConnect(mainView(_)), document.getElementById("container"))
  }

  def renderFilters(proxy: ModelProxy[RootModel]) = {
    val model = proxy.value
    val filters = model.publicationVisualization.filters
    val config = model.publicationVisualization.config
    val search = model.publicationVisualization.search
    def update(filters: (String) => Filters)(e: ReactEventI) = {
      proxy.dispatchCB(SetFilters(filters(e.target.value)))
    }

    <.div( // <.div("Title: ", <.input(
    //   ^.`type` := "text", // ^.value := search.title,
    //   // ^.onChange --> Callback.empty,
    //   ^.onKeyPress ==> ((e: ReactKeyboardEventI) => {
    //     if (e.charCode == 13)
    //       proxy.dispatch(SetSearch(Search(title = e.target.value)))
    //     else
    //       Callback.empty
    //   })
    // )),
    // filters.filters.map {
    //   case f: KeywordFilter =>
    //     <.div("Keyword:", <.input(^.`type` := "text", ^.value := f.query,
    //       ^.onChange ==> update(v => filters.copy(keyword = KeywordFilter(v)))))
    //   case f: AuthorFilter =>
    //     <.div("Author:", <.input(^.`type` := "text", ^.value := f.query,
    //       ^.onChange ==> update(v => filters.copy(author = AuthorFilter(v)))))
    //   case f: LimitFilter =>
    //     <.div("Limit:", <.input(^.`type` := "number", ^.value := f.limit,
    //       ^.onChange ==> update(v => filters.copy(limit = LimitFilter(v.toInt.abs)))))
    //   case f => <.div(f.toString)
    // }
    )
  }

  val mainView = ReactComponentB[ModelProxy[RootModel]]("MainView")
    .render_P { proxy =>
      <.div(
        ^.position := "absolute",
        ^.top := "0",
        ^.left := "0",
        ^.width := "100%",
        ^.height := "100%",
        ^.zIndex := "-1",
        <.div(
          proxy.wrap(m => m)(v => GraphView(v.value.publicationVisualization.graph, Vec2(400, 400), Some(
            GraphConfig(v.value.publicationVisualization.config, v.value.hoveredVertex, v.value.highlightedVertices)
          )))
        )
      )
    }
    .build

  def AjaxGetByteBuffer(url: String): Future[ByteBuffer] = {
    Ajax.get(
      url,
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(xhr => TypedArrayBuffer.wrap(xhr.response.asInstanceOf[ArrayBuffer]))
  }
}
