package tigrs

import boopickle.Default._

import scala.io.Source
import collection.mutable

import scala.xml.XML
import scala.xml.pull._
import scala.xml._

object Main extends App {
  def parsePublications = Global.faculties.par.flatMap { faculty =>
    // println(s"parsing $faculty.xml... ")
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
        val children = mutable.ArrayBuffer[Node]()

        while (it.hasNext) {
          it.next match {
            case EvElemStart(_, t, a, _) => {
              children += subTree(t, a)
            }
            case EvText(t) => {
              children += Text(t)
            }
            case EvElemEnd(_, t) => {
              return new Elem(null, tag, attrs, xml.TopScope, false, children: _*)
            }
            case _ =>
          }
        }
        return null // this shouldn't happen with good XML
      }

      try {
        val pubs = ModsParser.xmlToPublications(slurp("mods"))
        println(s"$faculty.xml: ${pubs.size} publications")
        pubs
      } catch {
        case e: Throwable =>
          println(s"error: ${e.getMessage}")
          Nil
      }
    } else {
      println(s"file does not exist")
      Nil
    }
  }.seq

  def filterByIkz(publications: Seq[tigrs.Publication], ikz: String): Seq[tigrs.Publication] = publications.filter(_.owner.map(_.ikz).toSeq.flatten contains ikz)

  val publications = {
    val pubs = parsePublications
    println("removing duplicates")
    val distinctPubs = pubs.map(p => p.recordId -> p).toMap.values.toSeq
    println(s"serializing ${distinctPubs.size} publications into data/fakall.boo ...")
    pickleIntoFile(distinctPubs, "data/fakall.boo")

    // println(s"serializing ${distinctPubs.size} publications into data/fakall.json ...")
    // pickleIntoJsonFile(distinctPubs, "data/fakall.json")

    distinctPubs
  }
  // val publications = loadPubData
  val ikzs: Seq[String] = publications.flatMap(_.owner.toSeq.flatMap(_.ikz)).distinct
  pickleIkzListIntoFile(ikzs, "data/fakall.ikzlist.boo")
  var done = 0
  for (ikz <- ikzs.par) {
    pickleIntoFile(filterByIkz(publications, ikz), s"data/fakall.ikz.$ikz.boo")
    done += 1
    print(s"institute: $done / ${ikzs.size}       \r")
  }
  println()

  // implicit def pickleState = new PickleState(new boopickle.EncoderSize, false, false)
  import PublicationPickler._

  def pickleIntoFile(data: Seq[Publication], file: String) { writeBufToFile(Pickle.intoBytes(data), file) }
  def pickleIkzListIntoFile(data: Seq[String], file: String) { writeBufToFile(Pickle.intoBytes(data), file) }

  def writeBufToFile(buf: java.nio.ByteBuffer, file: String) {
    import java.io.File
    import java.io.FileOutputStream
    val channel = new FileOutputStream(new File(file), false).getChannel()
    channel.write(buf)
    channel.close()
  }

  def loadPubData: Seq[Publication] = {
    import java.io.RandomAccessFile
    import java.nio.ByteBuffer
    import java.nio.channels.FileChannel
    val aFile = new RandomAccessFile("data/fakall.boo", "r")
    val inChannel = aFile.getChannel()
    val fileSize = inChannel.size()
    val buffer = ByteBuffer.allocate(fileSize.toInt)
    inChannel.read(buffer)
    buffer.flip()

    import PublicationPickler._
    val pubs = Unpickle[Seq[Publication]].fromBytes(buffer)

    inChannel.close()
    aFile.close()

    pubs
  }

  def pickleIntoJsonFile(data: Seq[Publication], file: String) {
    import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
    import java.io._

    val json = data.asJson.spaces2
    Some(new PrintWriter(file)).foreach { p => p.write(json); p.close }
  }

}
