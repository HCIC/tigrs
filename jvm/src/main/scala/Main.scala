package tigrs

import boopickle.Default._

import scala.io.Source
import collection.mutable

import scala.xml.XML
import scala.xml.pull._
import scala.xml._

object Main extends App {
  val publications = Publications(Global.faculties.flatMap { faculty =>
    print(s"parsing $faculty.xml... "); Console.flush()
    val xmlFile = s"data/$faculty.xml"
    if (new java.io.File(xmlFile).exists) {
      val it = new XMLEventReader(Source.fromFile(xmlFile))

      def slurp(tag: String): Iterator[Node] = {
        it.flatMap {
          case EvElemStart(pre, `tag`, attrs, _) => Some(subTree(tag, attrs))
          case _ => None
        }
      }

      def subTree(tag: String, attrs: MetaData): Node = {
        var children = mutable.ArrayBuffer[Node]()

        while (it.hasNext) {
          it.next match {
            case EvElemStart(_, t, a, _) => {
              children += subTree(t, a)
            }
            case EvText(t) => {
              children += Text(t)
            }
            case EvElemEnd(_, t) => {
              return new Elem(null, tag, attrs, xml.TopScope, children: _*)
            }
            case _ =>
          }
        }
        return null // this shouldn't happen with good XML
      }

      val pubs = ModsParser.xmlToPublications(slurp("mods"))
      println(s"${pubs.publications.size} publications")
      pubs.publications
    } else {
      println(s"file does not exist")
      Nil
    }
  })

  println("removing duplicates")
  val distinctPubs = publications.publications.map(p => p.recordId -> p).toMap.values.toSeq
  println(s"serializing ${distinctPubs.size} publication data into data/fakall.boo ...")
  pickleIntoFile(Publications(distinctPubs), "data/fakall.boo")

  def pickleIntoFile(data: Publications, file: String) {
    import java.io.File
    import java.io.FileOutputStream
    val channel = new FileOutputStream(new File(file), false).getChannel()

    implicit def pickleState = new PickleState(new boopickle.EncoderSize, false, false)
    import PublicationPickler._
    val buf = Pickle.intoBytes(data)

    channel.write(buf)
    channel.close()
  }
}
