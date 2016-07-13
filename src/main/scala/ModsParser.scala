package tigrs

import scala.scalajs.js
import org.scalajs.dom._
import domPatterns._

object ModsParser {
  def xmlToPublications(tree: Document, limit: Int = Integer.MAX_VALUE): Publications = {
    def extractTitle: PartialFunction[Node, String] = { case NodeEx("titleInfo", _, _, Seq(_, NodeEx("title", _, title, _), _)) => title }
    val extractAuthor: PartialFunction[Node, (String, String, Int)] = {
      case NodeEx("name", Seq(("type", "personal")), _, Seq(_,
        NodeEx("namePart", _, author, _), _,
        NodeEx("namePart", _, termsOfAdress, _), _,
        role, _,
        NodeEx("nameIdentifier", _, nameIdentifier, _), _
        )
        ) =>
        (author, nameIdentifier, termsOfAdress.toInt)
    }
    def extractKeywords: PartialFunction[Node, Seq[String]] = { case NodeEx("subject", _, _, Seq(_, NodeEx("topic", _, keywords, _), _)) => keywords.split(",").flatMap(_.split("/")).flatMap(_.split(";")).map(_.trim) }
    def extractConference: PartialFunction[Node, Conference] = {
      case NodeEx("name", Seq(("type", "conference")), _, Seq(_, NodeEx("namePart", _, name, _), _*)) =>
        Conference(name)
    }
    def extractJournal: PartialFunction[Node, Journal] = {
      case NodeEx("relatedItem", Seq(("type", "host")), _, Seq(_, NodeEx("titleInfo", _, _, Seq(_, NodeEx("title", _, name, _), _)), _*)) =>
        Journal(name)
    }
    def extractSeries: PartialFunction[Node, Series] = {
      case NodeEx("relatedItem", Seq(("type", "series")), _, Seq(_, NodeEx("titleInfo", _, _, Seq(_, NodeEx("title", _, name, _), _)), _*)) =>
        Series(name)
    }
    def extractOrigin: PartialFunction[Node, Origin] = {
      case NodeEx("originInfo", _, _, childNodes) =>
        val date = childNodes.find(_.nodeName == "dateIssued").get.textContent
        val publisher = childNodes.find(_.nodeName == "publisher").map(_.textContent)
        Origin(date, publisher)
    }
    def extractUri: PartialFunction[Node, String] = {
      case NodeEx("identifier", Seq(("type", "uri")), uri, _) => uri
    }

    def extractRecordId: PartialFunction[Node, String] = {
      case NodeEx("recordInfo", _, _, childNodes) =>
        val recordId = childNodes.find(_.nodeName == "recordIdentifier").get.textContent
        recordId
    }

    def extractOwner: PartialFunction[Node, Institute] = {
      case NodeEx("location", _, _, locations) =>
        Institute(
          locations.collect {
            case NodeEx("physicalLocation", Seq(("type", "collection")), ikz, _) => ikz
          }
        )
    }

    def extractProject: PartialFunction[Node, Project] = {
      case NodeEx("note", Seq(_, ("type", "funding"), ("xlink:href", id)), name, _) =>
        Project(id, name)
    }

    val publications = tree.documentElement.childNodes.take(limit).collect {
      case mods @ NodeEx("mods", _, _, entries) if entries.collectFirst(extractTitle).isDefined =>
        try {
          val title = entries.collectFirst(extractTitle).get
          val authors = entries.collect(extractAuthor).toList.sortBy {
            case (_, id, termsOfAdress) => termsOfAdress
          }.map { case (name, id, _) => Author(id, name) }
          val keywords = entries.collectFirst(extractKeywords).getOrElse(Nil)
          val outlet = entries.collectFirst(extractConference orElse extractJournal orElse extractSeries)
          val origin = entries.collectFirst(extractOrigin) match {
            case Some(origin) => origin
            case _ => println(mods.toString); Origin("", None)
          }
          val uri = entries.collectFirst(extractUri)
          val recordId = entries.collectFirst(extractRecordId).get
          val owner = entries.collectFirst(extractOwner)
          val projects = entries.collect(extractProject)

          Publication(title, authors, keywords, outlet, origin, uri, recordId, owner, projects)
        } catch {
          case e: Exception =>
            console.warn(e.getMessage)
            console.warn("could not parse:", mods.asInstanceOf[js.Any])
            throw e
        }
    }.toSeq
    // println(publications.take(10).mkString("\n\n"))
    Publications(publications)
  }

}
