package tigrs

import pharg.{DirectedGraph, Edge}

package graph {
  sealed trait Vertex
  case class Author(id: String) extends Vertex
  case class Publication(id: Int) extends Vertex
}

package object graph {
  def filterByIkz(publications: Seq[tigrs.Publication], ikz: String): Seq[tigrs.Publication] = publications.filter(_.owner.map(_.ikz).toSeq.flatten contains ikz)

  def pubGraph(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {

    val authors = publications.flatMap(_.authors).map(a => a.id -> Author(a.id)).toMap
    val pubs = publications.map(p => Publication(p.recordId))
    // val outlets = publications.flatMap(_.outlet).distinct
    // val projects = publications.flatMap(_.projects).distinct
    // val keywords = publications.flatMap(_.keywords).distinct

    val vertices = authors.values ++ pubs

    val edges: Seq[Edge[Vertex]] = publications.flatMap { p =>
      p.authors.map(a => Edge[Vertex](authors(a.id), Publication(p.recordId))) // ++
      // p.outlet.map(o => DiEdge(o, p)) ++
      // p.projects.map(pr => DiEdge(pr, p)) ++
      // p.keywords.map(k => DiEdge(k, p))
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }
}
