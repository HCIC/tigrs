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

import scalajs.js.typedarray._
import concurrent.{Future, Promise, Await}
import concurrent.duration.Duration
import java.nio.ByteBuffer

import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

import dom.ext.Ajax

import scala.util.{Try, Success, Failure}

import graph.Vertex

object Data {
  import scala.concurrent.ExecutionContext.Implicits.global

  def ajaxGetByteBuffer(url: String): Future[ByteBuffer] = {
    Ajax.get(
      url,
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(xhr => TypedArrayBuffer.wrap(xhr.response.asInstanceOf[ArrayBuffer]))
  }

  def downloadPublications(name: String): Future[Seq[Publication]] = {
    ajaxGetByteBuffer(s"data/$name.boo").map { byteBuffer =>
      println("downloading data...")
      import PublicationPickler._
      val ps = Unpickle[Seq[Publication]].fromBytes(byteBuffer)
      println(s"downloaded ${ps.size} publications.")
      ps
    }
  }

  def downloadIkzList: Future[Seq[String]] = {
    ajaxGetByteBuffer(s"data/fakall.ikzlist.boo").map { byteBuffer =>
      println("downloading ikz list...")
      val ikzs = Unpickle[Seq[String]].fromBytes(byteBuffer)
      ikzs
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

    AppCircuit.dispatch(SetIkz(Set(conf.ikz)))
    updateDimensions()

    downloadIkzList.onComplete {
      case Success(ikzs) => AppCircuit.dispatch(SetAvailableIkzList(ikzs))
      case Failure(e) => console.log(s"error downloading ikz list: $e")
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
      val m = proxy.value
      val vis = m.publicationVisualization
      <.div(
        ^.width := "100%",
        ^.height := "100%",
        GraphViewCanvas(
          GraphProps(
            vis.displayGraph,
            vis.dimensions,
            vis.simConfig,
            vis.visConfig,
            m.selectedVertices
          )
        ),
        vis.sliderWidget ?= configWidget(proxy),
        <.div(
          ^.position := "absolute",
          ^.top := "10px",
          ^.right := "10px",
          ^.display := "flex",
          ^.flexDirection := "column",
          ^.flexWrap := "wrap-reverse",
          m.selectedVertices.map(preview(_)),
          m.hoveredVertex.map(preview(_))
        )
      )
    }.build

  val ikzSelector = ReactComponentB[ModelProxy[PublicationVisualization]]("Ikz Selector")
    .render_P { proxy =>
      val vis = proxy.value
      val available = vis.availableIkzs
      val selected = vis.selectedIkzs

      <.div(
        "selected ikz:", <.br(),
        selected.toSeq.sorted.map(
          ikz => <.div(<.b(ikz), " (click to remove)", ^.cursor := "pointer",
            ^.key := ikz, ^.onClick ==> ((e: ReactEvent) => proxy.dispatchCB(SetIkz(selected - ikz))))
        ),
        "add ikz: ",
        <.select(
          ^.value := selected.headOption.getOrElse(""),
          ^.onChange ==> ((e: ReactEventI) => proxy.dispatchCB(SetIkz(selected + e.target.value))),
          <.option(),
          available.sorted.map(ikz => <.option(^.value := ikz, ikz))
        )
      )
    }.build

  val configWidget = ReactComponentB[ModelProxy[RootModel]]("configWidget")
    .render_P { proxy =>
      val model = proxy.value
      val vis = model.publicationVisualization

      def configSlider[C <: Config](title: String, min: Double, max: Double, step: Double, config: C, lens: Lens[C, Double], sideEffect: C => Unit = (_: C) => {}) = {
        <.div(
          ^.display := "flex",
          ^.justifyContent := "flex-start",
          <.div(s"$title: ", ^.width := "130px"),
          <.input(
            ^.`type` := "range", ^.min := min, ^.max := max, ^.step := step, ^.value := lens.get(config), ^.title := lens.get(config),
            ^.onChange ==> ((e: ReactEventI) => {
              val newConfig = lens.set(config)(e.target.value.toDouble)
              sideEffect(newConfig)
              proxy.dispatchCB(SetConfig(newConfig))
            })
          ),
          lens.get(config)
        )
      }
      <.div(
        ^.position := "absolute",
        ^.top := "10px",
        ^.left := "10px",
        ^.background := "white",
        ^.border := "1px solid #DDD",
        ^.padding := "10px",
        <.div(
          ^.display := "flex",
          ^.flex := "1 1 auto",
          <.div(
            proxy.wrap(_.publicationVisualization)(v => ikzSelector(v)),
            <.hr(),
            configSlider("Merge Authors", 0.01, 1.0, 0.01, vis.graphConfig, lens[GraphConfig] >> 'pubSimilarity,
              (c: GraphConfig) => Future { SetDisplayGraph(tigrs.graph.mergedGraph(c.pubSimilarity, c.authorSimilarity, c.fractionalCounting)(vis.publications)) }.foreach { a => AppCircuit.dispatch(a) }),
            configSlider("Merge Pubs", 0.01, 1.0, 0.01, vis.graphConfig, lens[GraphConfig] >> 'authorSimilarity,
              (c: GraphConfig) => Future { SetDisplayGraph(tigrs.graph.mergedGraph(c.pubSimilarity, c.authorSimilarity, c.fractionalCounting)(vis.publications)) }.foreach { a => AppCircuit.dispatch(a) }),
            <.div(
              ^.display := "flex",
              ^.justifyContent := "flex-start",
              s"Fract. counting: ",
              <.input(^.`type` := "checkbox", ^.checked := vis.graphConfig.fractionalCounting,
                ^.onChange ==> ((e: ReactEventI) => {
                  val checked = e.target.checked
                  Future { SetDisplayGraph(tigrs.graph.mergedGraph(vis.graphConfig.pubSimilarity, vis.graphConfig.authorSimilarity, checked)(vis.publications)) }.foreach { a => AppCircuit.dispatch(a) }
                  proxy.dispatchCB(SetConfig(vis.graphConfig.copy(fractionalCounting = checked)))
                }))
            ),

            configSlider("Repel", 0, 200, 1, vis.simConfig, lens[SimulationConfig] >> 'repel),
            configSlider("Gravity", 0, 1, 0.01, vis.simConfig, lens[SimulationConfig] >> 'gravity),
            configSlider("LinkDistance", 1, 100, 1, vis.simConfig, lens[SimulationConfig] >> 'linkDistance),

            configSlider("minYear", vis.publicationsMinYear, vis.publicationsMaxYear, 1, vis.visConfig, lens[VisualizationConfig] >> 'minYear),
            configSlider("maxYear", vis.publicationsMinYear, vis.publicationsMaxYear, 1, vis.visConfig, lens[VisualizationConfig] >> 'maxYear),
            configSlider("RadiusOffset", 0, 20, 0.5, vis.visConfig, lens[VisualizationConfig] >> 'radiusOffset),
            configSlider("RadiusFactor", 0, 20, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'radiusFactor),
            configSlider("RadiusExponent", 0, 2, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'radiusExponent),
            configSlider("WidthOffset", 0, 10, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'widthOffset),
            configSlider("WidthFactor", 0, 10, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'widthFactor),
            configSlider("WidthExponent", 0, 2, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'widthExponent),

            configSlider("AuthorLabels", 0, 1, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'authorLabels),
            <.div(
              <.input(^.`type` := "text", ^.value := vis.visConfig.filter, ^.placeholder := "Filter Authors", ^.width := "100%",
                ^.onChange ==> ((e: ReactEventI) => proxy.dispatchCB(SetConfig(vis.visConfig.copy(filter = e.target.value)))))
            )
          )
        )
      )
    }.build

  val preview = ReactComponentB[Vertex]("Preview")
    .render_P { vertex =>
      import graph._
      <.div(
        ^.position := "relative",
        ^.background := "white",
        ^.border := "1px solid #DDD",
        ^.marginBottom := "10px",
        ^.padding := "10px",
        ^.paddingRight := "20px",
        ^.width := "400px",
        <.div(
          ^.position := "absolute",
          ^.top := "0",
          ^.right := "0",
          ^.width := "30px",
          ^.height := "30px",
          ^.display := "flex",
          ^.justifyContent := "center",
          ^.alignItems := "center",
          ^.color := "#888",
          ^.cursor := "pointer",
          "x",
          ^.onClick --> Callback { AppCircuit.dispatch(DeselectVertex(vertex)) }
        ),
        vertex match {
          case graph.PublicationSet(_, ps) =>
            <.div(
              ps.sortBy(_.origin.year).reverse.map { p =>
                <.div(
                  ^.key := p.recordId,
                  s"[${p.origin.year}] ",
                  <.a(p.title, ^.href := s"https://scholar.google.de/scholar?q=${p.title}", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search in Google Scholar")
                )
              },
              <.br(),
              ps.flatMap(p => p.authors).distinct.sortBy(_.name).map(a => <.div(a.name))
            )
          case graph.AuthorSet(_, as) =>
            <.div(
              as.map(a => <.div(^.key := a.id, <.b(a.name)))
            )
        }
      )
    }
    .build
}
