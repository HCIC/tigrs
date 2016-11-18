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

// import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}

import dom.ext.Ajax

@JSExport
object Database {
  import scala.concurrent.ExecutionContext.Implicits.global

  val Dexie = js.Dynamic.global.Dexie
  val elasticlunr = js.Dynamic.global.elasticlunr

  @JSExport
  val db = js.Dynamic.newInstance(Dexie)("publications_database")
  db.version(2).stores(js.Dynamic.literal("publications" -> "", "authors" -> "", "meta" -> ""))
  db.open()

  def downloadData: Future[Seq[Publication]] = Main.AjaxGetByteBuffer("data/fakall.ikz.080013.boo").map { byteBuffer =>
    println("downloading publication data...")
    import PublicationPickler._
    val publications = Unpickle[Seq[Publication]].fromBytes(byteBuffer)
    println(s"downloaded ${publications.size} publications.")
    publications
  }

  def downloadGraph(name: String): Future[DirectedGraph[graph.Vertex]] = {
    Main.AjaxGetByteBuffer(s"data/$name.boo").map { byteBuffer =>
      println("downloading graph...")
      import PublicationPickler._
      val g = Unpickle[DirectedGraph[graph.Vertex]].fromBytes(byteBuffer)
      println(s"downloaded graph with ${g.vertices.size} vertices and ${g.edges.size} edges.")
      g
    }
    // val gen = downloadData.map { pubs =>
    //   println("generating graph...")
    //   // val pg = graph.authorCliqueGraphByPublication(pubs)
    //   // val g = cats.Functor[DirectedGraph].map(pg)(p => graph.Author(p.id): graph.Vertex)
    //   // val pg = graph.pubCliqueGraphByAuthor(pubs)
    //   // val g = cats.Functor[DirectedGraph].map(pg)(p => graph.Publication(p.recordId): graph.Vertex)
    //   val g = graph.pubCliqueMergedGraph(pubs, 0.5, 0.2)
    //   println(s"generated graph with ${g.vertices.size} vertices and ${g.edges.size} edges.")
    //   g
    // }
    // gen.onFailure { case e => console.log("error generationg graph: ", e.asInstanceOf[js.Any]); throw e }
    // gen
  }

  def readStoredData: Future[Unit] = db.publications.count().asInstanceOf[js.Promise[Int]].toFuture.map {
    case x if x > 0 =>
    case _ => throw new NoSuchElementException
  }

  def downloadIndex: Future[String] = Ajax.get("data/fakall.ikz.080013.index.json").map { xhr =>
    println("downloading index...")
    xhr.response.asInstanceOf[String]
  }

  def retrieveStoredIndex: Future[String] = {
    println("retrieving stored index...")
    val request = db.meta.get("searchindex").asInstanceOf[js.Promise[js.UndefOr[String]]].toFuture.map(_.toOption).map {
      case Some(index) => index
      case _ => throw new NoSuchElementException
    }
    request.onSuccess { case _ => console.log("successfully retrieved index.") }
    // request.onFailure { case e => console.log("error retrieving index from storage: ", e.asInstanceOf[js.Any]) }
    request
  }

  def loadIndex(_index: String): js.Dynamic = {
    val index = JSON.parse(_index).asInstanceOf[js.Dynamic]
    println("loading index...")
    elasticlunr.Index.load(index)
  }

  def storeIndex(index: js.Any): Future[Unit] = {
    println("storing index...")
    val storing = db.meta.put(index, "searchindex").asInstanceOf[js.Promise[js.Any]].toFuture
    storing.onSuccess { case _ => console.log("successfully stored index.") }
    storing.onFailure { case e => console.log("error storing index: ", e.asInstanceOf[js.Any]) }
    storing.map { _ => Unit }
  }

  def storePublications(publications: Seq[Publication]): Future[Unit] = {
    println("storing publications...")

    val items = publications.toJSArray.map { p =>
      import PublicationPickler._
      val data = Pickle.intoBytes(p)
      data.typedArray().subarray(data.position, data.limit)
    }.toJSArray

    val keys = publications.map(_.recordId).toJSArray
    val storing = db.publications.bulkPut(items, keys).asInstanceOf[js.Promise[js.Any]].toFuture

    storing.onSuccess { case _ => console.log("successfully stored publications.") }
    storing.onFailure { case e => console.log("error storing publications: ", e.asInstanceOf[js.Any]) }
    storing.map { _ => Unit }
  }

  def storeAuthors(publications: Seq[Publication]): Future[Unit] = {
    println("storing authors...")

    val authors = publications.flatMap(_.authors).distinct
    val items = authors.toJSArray.map { p =>
      import PublicationPickler._
      val data = Pickle.intoBytes(p)
      data.typedArray().subarray(data.position, data.limit)
    }.toJSArray

    val keys = authors.map(_.id).toJSArray
    val storing = db.authors.bulkPut(items, keys).asInstanceOf[js.Promise[js.Any]].toFuture

    storing.onSuccess { case _ => console.log("successfully stored authors.") }
    storing.onFailure { case e => console.log("error storing authors: ", e.asInstanceOf[js.Any]) }
    storing.map { _ => Unit }
  }

  val index: Future[js.Any] = async {
    val ensureData: Future[_] = readStoredData.recoverWith { case _ => (downloadData flatMap storePublications) zip (downloadData flatMap storeAuthors) }
    ensureData.onFailure { case e => console.log("data error: ", e.asInstanceOf[js.Any]) }
    val index = retrieveStoredIndex.recoverWith {
      case _ =>
        val downloaded = downloadIndex
        downloaded onSuccess { case i => storeIndex(i) }
        downloaded
    } map (loadIndex _)

    await(ensureData)
    println("data ready.")
    val i = await(index)
    println("index ready.")
    i
  }

  def lookupPublication(recordId: Int): Future[Publication] = {
    val resultDataF = db.publications.where(":id").applyDynamic("equals")(recordId).first().asInstanceOf[js.Promise[Int8Array]].toFuture
    resultDataF.map { data =>
      import PublicationPickler._
      Unpickle[Publication].fromBytes(TypedArrayBuffer.wrap(data))
    }
  }

  def lookupAuthor(id: String): Future[Author] = {
    val resultDataF = db.authors.where(":id").applyDynamic("equals")(id).first().asInstanceOf[js.Promise[Int8Array]].toFuture
    resultDataF.map { data =>
      import PublicationPickler._
      Unpickle[Author].fromBytes(TypedArrayBuffer.wrap(data))
    }
  }

  def lookupPublications(recordIds: Iterable[Int]): Future[Seq[Publication]] = {
    val resultDataF = db.publications.where(":id").anyOf(recordIds).toArray().asInstanceOf[js.Promise[js.Array[Int8Array]]].toFuture
    resultDataF.map { resultData =>
      resultData.map { data =>
        import PublicationPickler._
        Unpickle[Publication].fromBytes(TypedArrayBuffer.wrap(data))
      }
    }
  }

  def lookupAuthors(ids: Iterable[String]): Future[Seq[Author]] = {
    val resultDataF = db.authors.where(":id").anyOf(ids).toArray().asInstanceOf[js.Promise[js.Array[Int8Array]]].toFuture
    resultDataF.map { resultData =>
      resultData.map { data =>
        import PublicationPickler._
        Unpickle[Author].fromBytes(TypedArrayBuffer.wrap(data))
      }
    }
  }

  def search(search: Search): Future[Seq[Publication]] = {
    index.flatMap { index =>
      def obj = js.Dynamic.literal
      val searchConfig = obj(
        fields = obj(
          title = obj(boost = 1)
        ),
        expand = true,
        bool = "AND"
      )
      val result = index.asInstanceOf[js.Dynamic].search(search.title, searchConfig).asInstanceOf[js.Array[js.Dynamic]]
      val keys = result.map((r: js.Dynamic) => r.ref.asInstanceOf[String].toInt)
      lookupPublications(keys)
      // val resultMapF: Future[Map[String, Publication]] = resultDataF.map {
      //   _.map { data =>
      //     import PublicationPickler._
      //     val publication = Unpickle[Publication].fromBytes(TypedArrayBuffer.wrap(data))
      //     publication.recordId -> publication
      //   }.toMap
      // }

      // for (resultMap <- resultMapF) yield {
      //   Publications(result.map { r => r.score -> resultMap(r.ref.asInstanceOf[String]) })
      // }
    }
  }

  // def toJSObject(o: AnyRef): js.Object = {
  //   o match {
  //     case Publication(title, authors, keywords, outlet, origin, uri, recordId, owner, projects) =>
  //       js.Dynamic.literal(
  //         title = title,
  //         authors = toJSObject(authors),
  //         keywords = toJSObject(keywords),
  //         outlet = outlet.map(toJSObject).orUndefined,
  //         origin = toJSObject(origin),
  //         uri = uri.map(toJSObject).orUndefined,
  //         recordId = recordId,
  //         owner = owner.map(toJSObject).orUndefined,
  //         projects = toJSObject(projects)
  //       )
  //     case Institute(ikz) => js.Dynamic.literal(ikz = ikz)
  //     case Project(id, name) => js.Dynamic.literal(id = id, name = name)
  //     case Keyword(keyword) => js.Dynamic.literal(keyword = keyword)
  //     case Origin(date, publisher) => js.Dynamic.literal(date = date, publisher = publisher.orUndefined)
  //     case Author(id, name) => js.Dynamic.literal(id = id, name = name)
  //     case Conference(name) => js.Dynamic.literal(name = name)
  //     case Journal(name) => js.Dynamic.literal(name = name)
  //     case Series(name) => js.Dynamic.literal(name = name)
  //     case xs: Seq[_] => xs.toJSArray
  //   }
  // }
}

import Database._

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
              proxy.dispatch(SetConfig(newConfig)) >> {
                additionalDispatch match {
                  case Some(f) => proxy.dispatch(f(newConfig))
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
    Database // init database

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
      proxy.dispatch(SetFilters(filters(e.target.value)))
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
