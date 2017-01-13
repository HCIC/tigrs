package tigrs

import scala.xml._

object ModsParser {
  val numPattern = "\\d+".r
  def xmlToPublications(chunks: Iterator[Node], limit: Int = Integer.MAX_VALUE): Seq[Publication] = {
    val extractAuthor: PartialFunction[Node, Author] = {
      // <name type="personal">
      //   <namePart>Full Name</namePart>
      //   <namePart type="termsOfAddress">0</namePart>
      //   <nameIdentifier type="local">id</nameIdentifier>
      // </name>
      //
      // <name type="personal">
      //   <namePart>Full Name</namePart>
      //   <namePart type="termsOfAddress">1</namePart>
      //   <role>
      //     <roleTerm type="text">Thesis advisor</roleTerm>
      //   </role>
      //   <nameIdentifier type="local"/>
      // </name>
      //
      // <name type="personal">
      //   <namePart>Full Name</namePart>
      //   <namePart type="termsOfAddress">0</namePart>
      //   <nameIdentifier type="local"/>
      // </name>
      //
      // <name type="personal">
      //   <namePart>Full Name</namePart>
      //   <namePart type="termsOfAddress">0</namePart>
      //   <role>
      //     <roleTerm type="text">Editor</roleTerm>
      //   </role>
      //   <nameIdentifier type="local">id</nameIdentifier>
      // </name>
      //
      // fak07.xml-  <titleInfo>
      // fak07.xml:    <title>Smart Home Medical Technologies: Users’ Requirements for Conditional Acceptance</title>
      // fak07.xml-  </titleInfo>
      // fak07.xml-  <name type="personal">
      // fak07.xml-    <namePart>Himmel, Simon</namePart>
      // fak07.xml-    <namePart type="termsOfAddress">0</namePart>
      // fak07.xml-    <affiliation>rwth</affiliation>
      // fak07.xml-    <role>
      // fak07.xml-      <roleTerm type="text">Corresponding author</roleTerm>
      // fak07.xml-    </role>
      // fak07.xml-    <nameIdentifier type="local">P:(DE-82)IDM00576</nameIdentifier>
      // fak07.xml-  </name>

      //TODO: right now all Authors without nameIdentifier or "P:(DE-HGF)0" are skipped
      // case name @ <name>{ _* }</name> if (name \ "@type").text == "personal" =>
      //   name match {
      case name @ <name>{ _,
        <namePart>{ author }</namePart>, _,
        <namePart>{ termsOfAdress }</namePart>, _,
        role, _,
        affiliation, _,
        <nameIdentifier>{ nameIdentifier }</nameIdentifier>, _
        }</name> if (name \ "@type").text == "personal" && nameIdentifier.text != "P:(DE-HGF)0" =>
        Author(nameIdentifier.text, author.text, numPattern.findFirstIn(termsOfAdress.text).get.toInt)
      case name @ <name>{ _,
        <namePart>{ author }</namePart>, _,
        <namePart>{ termsOfAdress }</namePart>, _,
        role, _,
        <nameIdentifier>{ nameIdentifier }</nameIdentifier>, _
        }</name> if (name \ "@type").text == "personal" && nameIdentifier.text != "P:(DE-HGF)0" =>
        Author(nameIdentifier.text, author.text, numPattern.findFirstIn(termsOfAdress.text).get.toInt)
      case name @ <name>{ _,
        <namePart>{ author }</namePart>, _,
        <namePart>{ termsOfAdress }</namePart>, _,
        <nameIdentifier>{ nameIdentifier }</nameIdentifier>, _
        }</name> if (name \ "@type").text == "personal" && nameIdentifier.text != "P:(DE-HGF)0" =>
        Author(nameIdentifier.text, author.text, numPattern.findFirstIn(termsOfAdress.text).get.toInt)
      // }
    }

    def extractKeywords: PartialFunction[Node, Seq[Keyword]] = {
      case subject @ <subject>{ _* }</subject> =>
        val topics = (subject \ "topic").map(_.text)
        topics
          .flatMap(_.split(","))
          .flatMap(_.split("/"))
          .flatMap(_.split(";"))
          .map(s => Keyword(s.trim))
    }

    def extractConference: PartialFunction[Node, Conference] = {
      case name @ <name>{ _,
        <namePart>{ confName }</namePart>, _*
        }</name> if (name \ "@type").text == "conference" =>
        Conference(confName.text)
    }
    def extractJournal: PartialFunction[Node, Journal] = {
      case relatedItem @ <relatedItem>{ _,
        <titleInfo>{ _, <title>{ journalName }</title>, _ }</titleInfo>, _*
        }</relatedItem> if (relatedItem \ "@type").text == "host" =>
        Journal(journalName.text)
    }
    def extractSeries: PartialFunction[Node, Series] = {
      case relatedItem @ <relatedItem>{ _,
        <titleInfo>{ _, <title>{ seriesName }</title>, _ }</titleInfo>, _*
        }</relatedItem> if (relatedItem \ "@type").text == "series" =>
        Series(seriesName.text)
    }
    def extractOrigin: PartialFunction[Node, Origin] = {
      case originInfo @ <originInfo>{ childNodes @ _* }</originInfo> =>
        val date = (originInfo \ "dateIssued").text
        val publisher = childNodes collectFirst { case <publisher>{ publisher }</publisher> => publisher.text }
        Origin(date, publisher)
    }

    //TODO: does it work to parse uri?
    def extractUri: PartialFunction[Node, String] = {
      case identifier @ <identifier>{ uri }</identifier> if (identifier \ "@type").text == "uri" =>
        println(uri.text)
        uri.text
    }

    def extractRecordId: PartialFunction[Node, Int] = {
      case recordInfo @ <recordInfo>{ _* }</recordInfo> =>
        val recordId = (recordInfo \ "recordIdentifier").text
        recordId.toInt
    }

    def extractOwner: PartialFunction[Node, Institute] = {
      case <location>{ locations @ _* }</location> =>
        Institute(
          locations.collect {
            case <physicalLocation>{ ikz }</physicalLocation> => ikz.text
          }
        )
    }

    def extractProject: PartialFunction[Node, Project] = {
      case note @ <note>{ name }</note> if (note \ "@type").text == "funding" =>
        val id = (note \ "@xlink:href").text
        Project(id, name.text)
    }

    val publications: Seq[Publication] = chunks.collect {
      case mods @ <mods>{ entries @ _* }</mods> if (mods \ "titleInfo" \ "title").nonEmpty =>
        try {
          val title = (mods \ "titleInfo" \ "title").text
          val authors = entries.collect(extractAuthor).toSet

          val keywords = entries.collectFirst(extractKeywords).getOrElse(Nil)
          val outlet = entries.collectFirst(extractConference orElse extractJournal orElse extractSeries)
          val origin = entries.collectFirst(extractOrigin).get
          val uri = entries.collectFirst(extractUri)
          val recordId = entries.collectFirst(extractRecordId).get
          val owner = entries.collectFirst(extractOwner)
          val projects = entries.collect(extractProject)

          // Publication(title, authors, keywords, outlet, origin, uri, recordId, owner, projects)
          Publication(recordId, title, origin, authors, owner)
        } catch {
          case e: Exception =>
            println(e.getMessage)
            println("could not parse:")
            println(mods)
            throw e
        }
    }.toVector
    publications
  }

}
