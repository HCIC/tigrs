package tigrs

import scala.scalajs.js
import scala.scalajs.js.JSApp
import js.Dynamic.{global => g}
import js.DynamicImplicits._
import boopickle.Default._
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._
import scala.scalajs.js.typedarray.Uint8Array

object TutorialApp extends JSApp {
  lazy val fs = g.require("fs")
  lazy val elasticlunr = g.require("elasticlunr")

  def main(): Unit = {
    val index = createIndex(readPublications)

    println("writing index to data/fakall.boo.json...")
    fs.writeFile("data/fakall.index.json", index.toJSON())
  }

  def createIndex(pubs: Publications): js.Dynamic = {
    println("creating search index...")
    val index = elasticlunr()
    index.addField("title")
    index.setRef("recordId")
    index.saveDocument(false)

    for (pub <- pubs.publications) {
      index.addDoc(pub.asInstanceOf[js.Any])
    }

    println(index._fields)

    index
  }

  lazy val readPublications: Publications = {
    println("reading data/fakall.boo...")
    val buf = TypedArrayBuffer.wrap(fs.readFileSync("data/fakall.boo").asInstanceOf[ArrayBuffer])

    println("decoding...")
    import PublicationPickler._
    Unpickle[Publications].fromBytes(buf)
  }
}
