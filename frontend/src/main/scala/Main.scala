package tigrs

import collection.mutable

import collection.breakOut
import scala.scalajs.js
import scala.scalajs.js.{JSApp, JSON}
import js.timers.{setTimeout, clearTimeout, SetTimeoutHandle}
import js.JSConverters._
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom._
import scala.scalajs.js.annotation._
import org.scalajs.dom.ext.KeyCode
import window.localStorage
import window.screen
import scala.scalajs.js.Dynamic.global
import scala.annotation.meta.field

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import diode._
import diode.ActionResult.ModelUpdate
import diode.react._

import boopickle.Default._

import cats.syntax.either._
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

import graph.{Vertex, PublicationSet, AuthorSet}
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

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
    console.log("loading settings from local storage...")
    val visConfig = decode[VisualizationConfig](localStorage.getItem("visConfig"))
    val simConfig = decode[SimulationConfig](localStorage.getItem("simConfig"))
    val graphConfig = decode[GraphConfig](localStorage.getItem("graphConfig"))
    val selectedIkzs = decode[List[String]](localStorage.getItem("selectedIkzs"))

    for (vc <- visConfig; sc <- simConfig; gc <- graphConfig) {
      console.log("settings found, applying...")
      AppCircuit.dispatch(ActionBatch(
        SetConfig(vc.copy(filter = "")),
        SetConfig(sc),
        SetConfig(gc)
      ))
    }

    if (window.location.hash.isEmpty) {

      // console.log("no hash supplied, using defaults.")
      val defaults = UrlConfig()
      AppCircuit.dispatch(ActionBatch(
        SetIkz(selectedIkzs.getOrElse(defaults.ikz)),

        ShowSettings(defaults.showSettings),
        ShowIkzSelector(defaults.showIkzSelector)
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

  val dataToggle = "data-toggle".reactAttr
  val dataPlacement = "data-placement".reactAttr
  val dataHtml = "data-html".reactAttr

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
        )
      )
    }.build

  val ikzSelector = ReactComponentB[ModelProxy[PublicationVisualization]]("Ikz Selector")
    .render_P { proxy =>
      val vis = proxy.value
      val available = vis.availableIkzs
      val selected = vis.selectedIkzs

      <.div(
        <.h4("Institutes"),
        <.h5(selected.toSeq.sorted.map(
          ikz => <.span(<.span(^.`class` := "badge badge-primary", ikz, ^.key := ikz,
            <.span(" \u00D7 ", ^.color := "white", ^.title := "remove", ^.cursor := "pointer",
              ^.onClick ==> ((e: ReactEvent) => proxy.dispatchCB(SetIkz(selected diff List(ikz)))))), " ")
        )),
        <.select(
          ^.value := "",
          ^.onChange ==> ((e: ReactEventI) => {
            proxy.dispatchCB(SetIkz(selected :+ e.target.value))
          }),
          ^.`class` := "form-control form-control-sm",
          <.option(^.value := "", "Add Institute"),
          available.sorted.map(ikz => <.option(^.value := ikz, ikz))
        )
      )
    }.build

  var saveSuccessMsgTimeout: SetTimeoutHandle = setTimeout(0) { () }
  val settingsWidget = ReactComponentB[ModelProxy[RootModel]]("settingsWidget")
    .render_P { proxy =>
      val model = proxy.value
      val vis = model.publicationVisualization

      def settingsSlider[C <: Config](title: String, description: String, min: Double, max: Double, step: Double, config: C, lens: Lens[C, Double], sideEffect: C => Unit = (_: C) => {}) = {
        <.div(

          ^.`class` := "row",
          <.label(s"$title: ", ^.`class` := "col-sm-5 col-form-label col-form-label-sm"),
          <.div(
            ^.`class` := "col-sm-4",
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
          ),
          <.div(^.`class` := "col-sm-1", ^.padding := 0,
            <.span("\u24d8", dataToggle := "tooltip", dataPlacement := "bottom", dataHtml := "true", ^.title := s"""<p align="left">$description</p>""", ^.cursor := "help"))
        )
      }
      <.div(
        ^.width := "400px",
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
            settingsSlider("Merge Authors", "The slider adjusts the degree of similarity needed for a merge. When moving the slider to the right, authors are only merged when all their publications have no other co-authors. Setting the slider to the left relaxes this criterion and merges “similar” authors.", 0.01, 1.0, 0.01, vis.graphConfig, lens[GraphConfig] >> 'pubSimilarity),
            settingsSlider("Merge Publ.", "The slider adjusts the degree of similarity needed for a merge. When moving the slider to the right, publications are only merged when all their authors have no other publications. Setting the slider to the left relaxes this criterion and merges “similar” publications.", 0.01, 1.0, 0.01, vis.graphConfig, lens[GraphConfig] >> 'authorSimilarity),
            settingsSlider("From", "Display only publications after this year.", vis.publicationsMinYear, vis.publicationsMaxYear, 1, vis.visConfig, lens[VisualizationConfig] >> 'minYear),
            settingsSlider("Until", "Display only publications before this year.", vis.publicationsMinYear, vis.publicationsMaxYear, 1, vis.visConfig, lens[VisualizationConfig] >> 'maxYear),

            settingsSlider("Author Names", "This slider adjusts the range of names to display. By increasing this slider, more authors are shown. Authors are shown in accordance with the numbers of paper they have co-authored.", 0, 1, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'authorLabels),

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
                  ^.`class` := "col-sm-3",
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
                ),
                <.div(^.`class` := "col-sm-1", ^.padding := 0,
                  <.span("\u24d8", dataToggle := "tooltip", dataPlacement := "bottom", dataHtml := "true", ^.title := s"""<p align="left">The size of circles and thickness of lines used in the visualisation is determined by the number of papers an author has written. Fractional counting counts one paper as 1/n if the paper as n authors. Full counting counts every paper as 1.</p>""", ^.cursor := "help"))
              ),

              settingsSlider("Radius Offset", "Radius settings adjust the circle sizes (nodes). Offset sets the minimal size in pixels for each circle.", 0, 20, 0.5, vis.visConfig, lens[VisualizationConfig] >> 'radiusOffset),
              settingsSlider("Radius Factor", "Radius settings adjust the circle sizes (nodes). Factor adjusts how much the amount of publications is amplified linearly in the size of each circle.", 0, 20, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'radiusFactor),
              settingsSlider("Radius Exponent", "Radius settings adjust the circle sizes (nodes). Exponent adjusts the exponent of how the publication count goes into the circle size. Values smaller then 1 make authors with less publications more visible. Values greater than 1 amplify authors with many publications.", 0, 2, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'radiusExponent),
              settingsSlider("Width Offset", "Width settings adjust the line widths (edges). Offset sets the minimal size in pixels for each line.", 0, 10, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'widthOffset),
              settingsSlider("Width Factor", "Width settings adjust the line widths (edges). Factor adjusts how much the amount of publications is amplified linearly in the width of each line.", 0, 10, 0.1, vis.visConfig, lens[VisualizationConfig] >> 'widthFactor),
              settingsSlider("Width Exponent", "Width settings adjust the line widths (edges). Exponent adjusts the exponent of how the publication count goes into the line width. Values smaller then 1 make connections with less publications more visible. Values greater than 1 amplify connections with many publications.", 0, 2, 0.001, vis.visConfig, lens[VisualizationConfig] >> 'widthExponent),

              settingsSlider("Repel", "This slider adjusts the strength of the repulsion force between unconnected nodes. This indirectly controls the size of the graph.", 0, 200, 1, vis.simConfig, lens[SimulationConfig] >> 'repel),
              settingsSlider("Gravity", "All nodes are drawn to the center of the canvas. This prevents individual unconnected nodes from flying out of reach. Strong gravity can enforce a circular shape of the overall graph.", 0, 1, 0.01, vis.simConfig, lens[SimulationConfig] >> 'gravity),
              settingsSlider("Link Distance", "This slider adjust the desired distance between nodes. The layout algorithm tries to achieve this distance between any two connected nodes.", 1, 100, 1, vis.simConfig, lens[SimulationConfig] >> 'linkDistance)
            ),
            <.div(
              ^.display := "flex",
              ^.justifyContent := "space-between",
              ^.marginTop := "10px",
              <.button(^.`class` := "btn btn-outline-danger btn-sm", "Reset Settings",
                ^.onClick --> Callback {
                  AppCircuit.dispatch(ActionBatch(
                    SetConfig(VisualizationConfig(minYear = vis.publicationsMinYear, maxYear = vis.publicationsMaxYear)),
                    SetConfig(SimulationConfig()),
                    SetConfig(GraphConfig()),
                    ImportHash
                  ))
                }),
              <.button(^.`class` := "btn btn-secondary btn-sm", "Save Settings",
                ^.onClick --> Callback {
                  // save settings to local storage
                  localStorage.setItem("visConfig", vis.visConfig.asJson.spaces2)
                  localStorage.setItem("simConfig", vis.simConfig.asJson.spaces2)
                  localStorage.setItem("graphConfig", vis.graphConfig.asJson.spaces2)
                  localStorage.setItem("selectedIkzs", vis.selectedIkzs.asJson.spaces2)
                  document.getElementById("save-feedback").asInstanceOf[HTMLElement].style.display = "block"
                  clearTimeout(saveSuccessMsgTimeout)
                  saveSuccessMsgTimeout = setTimeout(5000) { document.getElementById("save-feedback").asInstanceOf[HTMLElement].style.display = "none" }

                  // collect data for analysis
                  case class IkzList(ikz: Seq[String])
                  case class ScreenStats(
                    screenWidth: Double = screen.width,
                    screenHeight: Double = screen.height,
                    screenAvailableWidth: Double = screen.availWidth,
                    screenAvailableHeight: Double = screen.availHeight,
                    devicePixelRatio: Double = window.devicePixelRatio
                  )

                  val authors: Set[tigrs.Author] = vis.publications.flatMap(_.authors)(breakOut)
                  case class GraphStats(
                    publicationCount: Int = vis.publications.size,
                    publicationSetCount: Int = vis.displayGraph.vertices.count(_.isInstanceOf[PublicationSet]),
                    authorCount: Int = authors.size,
                    authorSetCount: Int = vis.displayGraph.vertices.count(_.isInstanceOf[AuthorSet]),
                    edgeCount: Int = vis.displayGraph.edges.size
                  )

                  case class BrowserInfo(val browser: String, val browserMajorVersion: Int)
                  val browser = js.Dynamic.global.detectBrowser()
                  val browserInfo = BrowserInfo(browser.browser.asInstanceOf[String], browser.browserMajorVersion.asInstanceOf[Int])

                  Ajax.put(
                    "settings.php",
                    headers = Map("Content-Type" -> "application/json"),
                    data = (
                      ScreenStats().asJson deepMerge
                      browserInfo.asJson deepMerge
                      vis.simConfig.asJson deepMerge
                      vis.visConfig.asJson deepMerge
                      vis.graphConfig.asJson deepMerge
                      GraphStats().asJson deepMerge
                      IkzList(vis.selectedIkzs).asJson
                    ).spaces2
                  )
                })
            ),
            <.div(
              ^.id := "save-feedback",
              ^.textAlign := "right",
              ^.display := "none",
              <.small("Settings saved successfully.")
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
                  <.a(p.title, ^.href := s"https://scholar.google.de/scholar?q=${p.title}", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search in Google Scholar"),
                <.a(
                  <.img(^.src:="ub.png", ^.marginBottom := 3, ^.marginLeft := 5),
                  ^.href := s"https://publications.rwth-aachen.de/record/${p.recordId}", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Open Publication in University Library")
                )
              },
              <.br(),
              ps.flatMap(p => p.authors).distinct.sortBy(_.name).map(a => <.div(
                <.a(a.name, ^.href := s"https://scholar.google.de/citations?mauthors=${a.name}&hl=en&view_op=search_authors", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search Author in Google Scholar"),
                <.a(
                  <.img(^.src:="ub.png", ^.marginBottom := 3, ^.marginLeft := 5),
                  ^.href := s"https://publications.rwth-aachen.de/search?ln=en&p=aid:${a.id}", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search Author in University Library")
              ))
            )
          case graph.AuthorSet(_, as) =>
            <.div(
              as.map(a => <.div(
                ^.key := a.id,
                <.a(a.name, ^.href := s"https://scholar.google.de/citations?mauthors=${a.name}&hl=en&view_op=search_authors", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search Author in Google Scholar"),
                <.a(
                  <.img(^.src:="ub.png", ^.marginBottom := 3, ^.marginLeft := 5),
                  ^.href := s"https://publications.rwth-aachen.de/search?ln=en&p=aid:${a.id}", ^.target := "_blank", ^.cursor := "pointer", ^.title := "Search Author in University Library")
              ))
            )
        }
      )
    }
    .build
}
