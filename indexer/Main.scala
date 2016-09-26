package tigrs

import scala.scalajs.js
import scala.scalajs.js.{JSApp, JSON}
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
    fs.writeFile("data/fakall.ikz.080025.index.json", JSON.stringify(index))
  }

  def createIndex(pubs: Seq[Publication]): js.Dynamic = {
    println("creating search index...")
    val index = elasticlunr()
    index.addField("title")
    index.setRef("recordId")
    index.saveDocument(false)

    for (pub <- pubs) {
      index.addDoc(pub.asInstanceOf[js.Any])
    }

    index
  }

  lazy val readPublications: Seq[Publication] = {
    println("reading data/fakall.ikz.080025.boo...")
    val buf = TypedArrayBuffer.wrap(fs.readFileSync("data/fakall.ikz.080025.boo").asInstanceOf[ArrayBuffer])

    println("decoding...")
    import PublicationPickler._
    Unpickle[Seq[Publication]].fromBytes(buf)
  }
}
