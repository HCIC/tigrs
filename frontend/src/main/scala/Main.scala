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

object Visualization {
  import scala.concurrent.ExecutionContext.Implicits.global

  @JSExportTopLevel("render")
  def render(renderTarget: dom.raw.Element) {
    if (window.location.hash.isEmpty) {
      console.log("no hash supplied, using defaults.")
      val defaults = UrlConfig()
      AppCircuit.dispatch(ActionBatch(
        SetIkz(defaults.ikz.toSet),
        ShowSettings(defaults.showSettings),
        ShowIkzSelector(defaults.showIkzSelector),
        SetConfig(GraphConfig(fractionalCounting = defaults.fractionalCounting))
      ))
    } else {
      AppCircuit.dispatch(ImportHash)
    }

    updateDimensions()

    downloadIkzList.onComplete {
      case Success(ikzs) => AppCircuit.dispatch(SetAvailableIkzList(ikzs))
      case Failure(e) => console.log(s"error downloading ikz list: $e")
    }

    window.addEventListener("resize", { e: Event => updateDimensions() })

    def updateDimensions() {
      val targetRect = renderTarget.getBoundingClientRect()
      val targetDim = Vec2(targetRect.width, targetRect.height)
      AppCircuit.dispatch(SetDimensions(targetDim))
    }

    val modelConnect = AppCircuit.connect(m => m)
    ReactDOM.render(modelConnect(widgetView(_)), renderTarget)
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
        vis.showSettings ?= settingsWidget(proxy),
        <.div(
          ^.position := "absolute",
          ^.top := "10px",
          ^.right := "10px",
          ^.display := "flex",
          ^.flexDirection := "column",
          m.selectedVertices.map(preview(_))
        // m.hoveredVertex.map(preview(_))
        )
      )
    }.build

  val dataToggle = "data-toggle".reactAttr

  val ikzSelector = ReactComponentB[ModelProxy[PublicationVisualization]]("Ikz Selector")
    .render_P { proxy =>
      val vis = proxy.value
      val available = vis.availableIkzs
      val selected = vis.selectedIkzs

      <.div(
        <.h4("Institutes"),
        <.h5(selected.toSeq.sorted.map(
          ikz => <.span(<.span(^.`class` := "badge badge-primary", ikz, ^.key := ikz, <.span(" \u00D7 ", ^.color := "white", ^.title := "remove", ^.cursor := "pointer", ^.onClick ==> ((e: ReactEvent) => proxy.dispatchCB(SetIkz(selected - ikz))))), " ")
        )),
        <.select(
          ^.value := "",
          ^.onChange ==> ((e: ReactEventI) => {
            proxy.dispatchCB(SetIkz(selected + e.target.value))
          }),
          ^.`class` := "form-control form-control-sm",
          <.option(^.value := "", "Add Institute"),
          available.sorted.map(ikz => <.option(^.value := ikz, ikz))
        )
      )
    }.build

  val settingsWidget = ReactComponentB[ModelProxy[RootModel]]("settingsWidget")
    .render_P { proxy =>
      val model = proxy.value
      val vis = model.publicationVisualization

      def settingsSlider[C <: Config](title: String, min: Double, max: Double, step: Double, config: C, lens: Lens[C, Double], sideEffect: C => Unit = (_: C) => {}) = {
        <.div(
          ^.`class` := "row",
          <.label(s"$title: ", ^.`class` := "col-sm-5 col-form-label col-form-label-sm"),
          <.div(
            ^.`class` := "col-sm-5",
            <.input(
              ^.`type` := "range", ^.min := min, ^.max := max, ^.step := step, ^.value := lens.get(config),
              ^.`class` := "form-control form-control-sm",
              ^.onChange ==> ((e: ReactEventI) => {
                val newConfig = lens.set(config)(e.target.value.toDouble)
                sideEffect(newConfig)
                proxy.dispatchCB(SetConfig(newConfig))
              })
            )
          ),
          <.div(
            ^.`class` := "col-sm-2",
            lens.get(config)
          )
        )
      }
      <.div(
        ^.width := "350px",
        ^.position := "absolute",
        ^.top := "10px",
        ^.left := "10px",
        ^.`class` := "alert show",
        ^.background := "white",
        ^.border := "1px solid #DDD",
        <.div(
          <.a(dataToggle := "collapse", ^.href := "#settings", "Settings", ^.display := "inline-block", ^.width := "100%"),
          <.div(
            ^.`class` := "collapse", ^.id := "settings",
            vis.showIkzSelector ?= proxy.wrap(_.publicationVisualization)(v => ikzSelector(v)),
            <.hr(),
            settingsSlider("Merge Authors", 0.01, 1.0, 0.01, vis.graphConfig, lens[GraphConfig] >> 'pubSimilarity),
            settingsSlider("Merge Publ.", 0.01, 1.0, 0.01, vis.graphConfig, lens[GraphConfig] >> 'authorSimilarity),
            settingsSlider("From", vis.publicationsMinYear, vis.publicationsMaxYear, 1, vis.visConfig, lens[VisualizationConfig] >> 'minYear),
            settingsSlider("Until", vis.publicationsMinYear, vis.publicationsMaxYear, 1, vis.visConfig, lens[VisualizationConfig] >> 'maxYear),

            settingsSlider("Author Names", 0, 1, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'authorLabels),

            <.div(
              ^.`class` := "row",
              <.div(
                ^.`class` := "col-sm-12",
                <.div(
                  ^.`class` := "input-group input-group-sm",
                  <.input(^.placeholder := "Search for Authors", ^.`type` := "search", ^.value := vis.visConfig.filter, ^.`class` := "form-control form-control-sm",
                    ^.onChange ==> ((e: ReactEventI) => proxy.dispatchCB(SetConfig(vis.visConfig.copy(filter = e.target.value))))),
                  <.span(^.`class` := "input-group-btn", <.button(^.`class` := "btn btn-secondary", "\u00D7", ^.onClick --> proxy.dispatchCB(SetConfig(vis.visConfig.copy(filter = "")))))
                )
              )
            ),

            <.hr(),
            <.a(dataToggle := "collapse", ^.href := "#advancedSettings", "Advanced Settings", ^.display := "inline-block", ^.width := "100%"),
            <.div(
              ^.`class` := "collapse", ^.id := "advancedSettings",
              <.div(
                ^.`class` := "row",
                <.div(^.`class` := "col-sm-8 col-form-label col-form-label-sm", s"Fractional Counting: "),
                <.div(
                  ^.`class` := "col-sm-4",
                  <.div(
                    ^.`class` := "form-check",
                    <.label(
                      ^.`class` := "form-check-label",
                      <.input(^.`type` := "checkbox", ^.checked := vis.graphConfig.fractionalCounting,
                        ^.`class` := "form-check-input",
                        ^.onChange ==> ((e: ReactEventI) => {
                          val checked = e.target.checked
                          proxy.dispatchCB(SetConfig(vis.graphConfig.copy(fractionalCounting = checked)))
                        }))
                    )
                  )
                )
              ),

              settingsSlider("Radius Offset", 0, 20, 0.5, vis.visConfig, lens[VisualizationConfig] >> 'radiusOffset),
              settingsSlider("Radius Factor", 0, 20, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'radiusFactor),
              settingsSlider("Radius Exponent", 0, 2, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'radiusExponent),
              settingsSlider("Width Offset", 0, 10, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'widthOffset),
              settingsSlider("Width Factor", 0, 10, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'widthFactor),
              settingsSlider("Width Exponent", 0, 2, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'widthExponent),

              settingsSlider("Repel", 0, 200, 1, vis.simConfig, lens[SimulationConfig] >> 'repel),
              settingsSlider("Gravity", 0, 1, 0.01, vis.simConfig, lens[SimulationConfig] >> 'gravity),
              settingsSlider("Link Distance", 1, 100, 1, vis.simConfig, lens[SimulationConfig] >> 'linkDistance)
            )
          )
        )
      )
    }.build

  val preview = ReactComponentB[Vertex]("Preview")
    .render_P { vertex =>
      import graph._
      <.div(
        ^.`class` := "alert alert-dismissable fade show",
        ^.position := "relative",
        ^.background := "white",
        ^.border := "1px solid #DDD",
        ^.marginBottom := "10px",
        ^.padding := "10px",
        ^.paddingRight := "20px",
        ^.width := "400px",
        <.button(
          ^.`type` := "button", ^.`class` := "close",
          <.span("\u00D7"), // times symbol
          ^.onClick --> Callback { AppCircuit.dispatch(DeselectVertex(vertex)) }
        ),
        vertex match {
          case graph.PublicationSet(_, ps) =>
            <.div(
              ps.map { p =>
                <.div(
                  ^.key := p.recordId,
                  s"${p.origin.year} ",
                  <.a(p.title, ^.href := s"https://scholar.google.de/scholar?q=${p.title}", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search in Google Scholar")
                )
              },
              <.br(),
              ps.flatMap(p => p.authors).distinct.sortBy(_.name).map(a => <.div(
                <.a(a.name, ^.href := s"https://scholar.google.de/citations?mauthors=${a.name}&hl=en&view_op=search_authors", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search Author in Google Scholar")
              ))
            )
          case graph.AuthorSet(_, as) =>
            <.div(
              as.map(a => <.div(
                ^.key := a.id,
                <.a(a.name, ^.href := s"https://scholar.google.de/citations?mauthors=${a.name}&hl=en&view_op=search_authors", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search Author in Google Scholar")
              ))
            )
        }
      )
    }
    .build
}
