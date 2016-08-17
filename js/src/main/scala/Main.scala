package tigrs

import collection.mutable

import scala.scalajs.js
import scala.scalajs.js.JSApp
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

import js.JSConverters._
import scalajs.js.typedarray._
import concurrent.{Future, Promise}
import java.nio.ByteBuffer

import org.scalajs.dom.idb.Database
import org.scalajs.dom.raw.IDBVersionChangeEvent

import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

import dom.ext.Ajax

object Main extends JSApp {

  val Dexie = js.Dynamic.global.Dexie
  val elasticlunr = js.Dynamic.global.elasticlunr
  def toJSObject(o: AnyRef): js.Object = {
    o match {
      case Publication(title, authors, keywords, outlet, origin, uri, recordId, owner, projects) =>
        js.Dynamic.literal(
          title = title,
          authors = toJSObject(authors),
          keywords = toJSObject(keywords),
          outlet = outlet.map(toJSObject).orUndefined,
          origin = toJSObject(origin),
          uri = uri.map(toJSObject).orUndefined,
          recordId = recordId,
          owner = owner.map(toJSObject).orUndefined,
          projects = toJSObject(projects)
        )
      case Institute(ikz) => js.Dynamic.literal(ikz = ikz)
      case Project(id, name) => js.Dynamic.literal(id = id, name = name)
      case Keyword(keyword) => js.Dynamic.literal(keyword = keyword)
      case Origin(date, publisher) => js.Dynamic.literal(date = date, publisher = publisher.orUndefined)
      case Author(id, name) => js.Dynamic.literal(id = id, name = name)
      case Conference(name) => js.Dynamic.literal(name = name)
      case Journal(name) => js.Dynamic.literal(name = name)
      case Series(name) => js.Dynamic.literal(name = name)
      case xs: Seq[_] => xs.toJSArray
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  def main() {
    val db = js.Dynamic.newInstance(Dexie)("publications_database")
    db.version(2).stores(js.Dynamic.literal("publications" -> "", "meta" -> ""))
    db.open()

    import scala.concurrent.ExecutionContext.Implicits.global
    val searchIndex: Future[js.Dynamic] = {
      val storedDataCount = db.publications.count().asInstanceOf[js.Promise[Int]].toFuture

      storedDataCount.filter(_ > 0).flatMap { _ =>
        println("loading index form local storage...")
        val indexRequest = db.meta.get("searchindex").asInstanceOf[js.Promise[js.Dynamic]].toFuture
        indexRequest.map { index => elasticlunr.Index.load(index) }
      }.recoverWith {
        case _ =>
          println(s"downloading publication data...")
          val downloadedData = AjaxGetByteBuffer("data/fakall.boo").map {
            case byteBuffer =>
              import PublicationPickler._
              val publications = Unpickle[Publications].fromBytes(byteBuffer)
              println(s"downloaded ${publications.publications.size} publications.")
              Publications(publications.publications.take(30000))
          }

          downloadedData.foreach { publications =>
            println("storing publications...")

            val items = publications.publications.map { p =>
              import PublicationPickler._
              val data = Pickle.intoBytes(p)
              data.typedArray().subarray(data.position, data.limit)
            }.toJSArray

            val keys = publications.publications.map(_.recordId).toJSArray

            db.publications.bulkPut(items, keys)
            println("stored all publications")
          }

          downloadedData.map { publications =>
            println("creating search index...")

            val index = elasticlunr()
            index.addField("title")
            index.setRef("recordId")
            index.saveDocument(false)

            for (pub <- publications.publications) {
              index.addDoc(toJSObject(pub))
            }
            val storing = db.meta.put(index.toJSON(), "searchindex").asInstanceOf[js.Promise[js.Any]].toFuture

            storing.onSuccess { case _ => console.log("successfully created and stored index.") }
            storing.onFailure { case e => console.log("error storing index: ", e.asInstanceOf[js.Any]) }
            index
          }
      }
    }

    searchIndex.onSuccess {
      case index =>
        println("successfully initiated search index.")
    }

    val dataRetrieval = for (index <- searchIndex) yield {
      println("query data...")
      val result = index.asInstanceOf[js.Dynamic].search("Helium").asInstanceOf[js.Array[js.Dynamic]]
      val keys = result.map((r: js.Dynamic) => r.ref)
      val resultDataF = db.publications.where(":id").anyOf(keys).toArray().asInstanceOf[js.Promise[js.Array[Int8Array]]].toFuture
      val resultMapF: Future[Map[String, Publication]] = resultDataF.map {
        _.map { data =>
          import PublicationPickler._
          val publication = Unpickle[Publication].fromBytes(TypedArrayBuffer.wrap(data))
          publication.recordId -> publication
        }.toMap
      }

      for (resultMap <- resultMapF) {
        val scoreMap = result.map { r => r.score -> resultMap(r.ref.asInstanceOf[String]) }
        println(scoreMap.mkString("\n"))
      }
    }

    dataRetrieval.onFailure {
      case e =>
        println(s"data dataRetrieval onfailure: $e")
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
