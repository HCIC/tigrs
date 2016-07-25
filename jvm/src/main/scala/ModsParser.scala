package tigrs

import scala.xml._

object ModsParser {
  def xmlToPublications(tree: Elem, limit: Int = Integer.MAX_VALUE): Publications = {
    val extractAuthor: PartialFunction[Node, (String, String, Int)] = {
      case name @ <name>{ _,
        <namePart>{ author }</namePart>, _,
        <namePart>{ termsOfAdress }</namePart>, _,
        role, _,
        <nameIdentifier>{ nameIdentifier }</nameIdentifier>, _
        }</name> if (name \ "@type").text == "personal" =>
        (author.text, nameIdentifier.text, termsOfAdress.text.toInt)
    }

    def extractKeywords: PartialFunction[Node, Seq[Keyword]] = {
      case subject @ <subject>{ _, <topic>{ keywords }</topic>, _ }</subject> =>
        keywords.text.split(",").flatMap(_.split("/")).flatMap(_.split(";")).map(s => Keyword(s.trim))
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

    def extractRecordId: PartialFunction[Node, String] = {
      case recordInfo @ <recordInfo>{ _* }</recordInfo> =>
        val recordId = (recordInfo \ "recordIdentifier").text
        recordId
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

    val publications: Seq[Publication] = (tree \ "mods").take(limit).collect {
      case mods @ <mods>{ entries @ _* }</mods> if (mods \ "titleInfo" \ "title").nonEmpty =>
        try {
          // println("--------------------------")
          // println(mods)
          val title = (mods \ "titleInfo" \ "title").text
          val authors = entries.collect(extractAuthor).toList.sortBy {
            case (_, id, termsOfAdress) => termsOfAdress
          }.map { case (name, id, _) => Author(id, name) }

          // println(title)
          // println(authors)
          val keywords = entries.collectFirst(extractKeywords).getOrElse(Nil)
          // println(keywords)
          val outlet = entries.collectFirst(extractConference orElse extractJournal orElse extractSeries)
          // println(outlet)
          val origin = entries.collectFirst(extractOrigin).get
          val uri = entries.collectFirst(extractUri)
          val recordId = entries.collectFirst(extractRecordId).get
          val owner = entries.collectFirst(extractOwner)
          val projects = entries.collect(extractProject)

          Publication(title, authors, keywords, outlet, origin, uri, recordId, owner, projects)
        } catch {
          case e: Exception =>
            println(e.getMessage)
            println("could not parse:", mods)
            throw e
        }
    }.toSeq
    // println(publications.take(10).mkString("\n\n"))
    Publications(publications)
  }

}
