package tigrs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

sealed trait PubVertex

case class Institute(ikz: Seq[String])
case class Project(id: String, name: String)

case class Publication(
  title: String,
  authors: Seq[Author],
  keywords: Seq[String] = Nil,
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
case class Conference(name: String) extends Outlet
case class Journal(name: String) extends Outlet
case class Series(name: String) extends Outlet

case class Publications(publications: Seq[Publication]) {
  def authors = publications.flatMap(_.authors).map(a => a.id -> a).toMap
  def outlets = publications.flatMap(_.outlet).distinct

  def toGraph: Graph[PubVertex, DiEdge] = {
    val authorToPublication = publications.flatMap(p => p.authors.map(a => DiEdge(authors(a.id), p)))
    val outletToPublication = publications.flatMap(p => p.outlet.map(o => DiEdge(o, p)))

    val edges = authorToPublication ++ outletToPublication
    val vertices = publications ++ authors.values ++ outlets
    Graph.from(vertices, edges)
  }
}
