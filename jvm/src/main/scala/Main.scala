package tigrs

import boopickle.Default._

import scala.io.Source

import scala.xml.XML

object Main extends App {
  val publications = Publications(Global.faculties.flatMap { faculty =>
    print(s"parsing $faculty.xml ... ")
    val xmlFile = s"data/$faculty.xml"
    val xml = XML.loadFile(xmlFile)
    val pubs = ModsParser.xmlToPublications(xml)
    println(s"${pubs.publications.size} publications")
    pubs.publications
  })

  pickleIntoFile(publications, "data/fakall.boo")

  def pickleIntoFile(data: Publications, file: String) {
    import java.io.File
    import java.io.FileOutputStream
    val channel = new FileOutputStream(new File(file), false).getChannel()
    val buf = Pickle.intoBytes(data)
    channel.write(buf)
    channel.close()
  }
}
