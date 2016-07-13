package tigrs

import boopickle.Default._

import scala.io.Source

import scala.xml.XML

object Main extends App {
  val xmlFile = "data/fak00.xml"
  // val xml = new XMLEventReader(Source.fromFile(xmlFile))
  val xml = XML.loadFile(xmlFile)

  val pubs = ModsParser.xmlToPublications(xml)

  // def pickleIntoFile(data: Publications, file: String) {
  //   import java.io.File
  //   import java.io.FileOutputStream
  //   val channel = new FileOutputStream(new File(file), false).getChannel()
  //   val buf = Pickle.intoBytes(data)
  //   channel.write(buf)
  //   channel.close()
  // }
}
