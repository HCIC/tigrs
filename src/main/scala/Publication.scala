package tigrs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

trait PubVertex

case class Author(id: String)(val name: String) extends PubVertex

trait Outlet
case class Conference(name: String) extends Outlet
case class Journal(name: String) extends Outlet
case class Series(name: String) extends Outlet

case class Origin(date: String, publisher: Option[String])

case class Publication(title: String, authors: Seq[Author], keyWords: Seq[String] = Nil, outlet: Option[Outlet], origin: Origin, uri: Option[String], recordId: String) extends PubVertex

case class Publications(publications: Seq[Publication]) {
  def authors = publications.flatMap(_.authors).distinct.map(a => a.id -> a).toMap

  def toAuthorPublicationGraph: Graph[PubVertex, DiEdge] = {
    val edges = publications.flatMap(p => p.authors.map(a => DiEdge(authors(a.id), p)))
    val vertices = publications ++ authors.values
    Graph.from(vertices, edges)
  }
}
