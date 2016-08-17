package tigrs

import scala.scalajs.js.annotation._

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

sealed trait PubVertex

case class Institute(ikz: Seq[String])
case class Project(id: String, name: String) extends PubVertex
case class Keyword(keyword: String) extends PubVertex

@JSExportAll
case class Publication(
  title: String,
  authors: Seq[Author],
  keywords: Seq[Keyword],
  outlet: Option[Outlet],
  origin: Origin,
  uri: Option[String],
  recordId: String,
  owner: Option[Institute],
  projects: Seq[Project]
) extends PubVertex

case class Origin(date: String, publisher: Option[String])

//TODO: role
case class Author(id: String, name: String) extends PubVertex

sealed trait Outlet extends PubVertex {
  def name: String
}

object PublicationPickler {
  import boopickle.Default._
  implicit val outletPickler = compositePickler[Outlet].
    addConcreteType[Conference].
    addConcreteType[Journal].
    addConcreteType[Series]
}

case class Conference(name: String) extends Outlet
case class Journal(name: String) extends Outlet
case class Series(name: String) extends Outlet

case class Publications(publications: Seq[Publication]) {
  lazy val authors = publications.flatMap(_.authors).map(a => a.id -> a).toMap
  lazy val outlets = publications.flatMap(_.outlet).distinct
  lazy val projects = publications.flatMap(_.projects).distinct
  lazy val keywords = publications.flatMap(_.keywords).distinct

  lazy val toGraph: Graph[PubVertex, DiEdge] = {
    val authorToPublication = publications.flatMap(p => p.authors.map(a => DiEdge(authors(a.id), p)))
    val outletToPublication = publications.flatMap(p => p.outlet.map(o => DiEdge(o, p)))
    val projectToPublication = publications.flatMap(p => p.projects.map(pr => DiEdge(pr, p)))
    val keywordToPublication = publications.flatMap(p => p.keywords.map(k => DiEdge(k, p)))

    val edges = authorToPublication ++ outletToPublication ++ projectToPublication ++ keywordToPublication
    val vertices = publications ++ authors.values ++ outlets ++ projects ++ keywords
    Graph.from(vertices, edges)
  }
}
