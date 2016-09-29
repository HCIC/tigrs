package tigrs

import pharg.{DirectedGraph, Edge}

package graph {
  sealed trait Vertex
  case class Author(id: String) extends Vertex
  case class Publication(id: Int) extends Vertex
  case class PublicationSet(ids: Set[Int]) extends Vertex
  case class Keyword(keyword: String) extends Vertex
  case class Project(id: String) extends Vertex
  case class Outlet(name: String) extends Vertex
}

package object graph {
  def filterByIkz(publications: Seq[tigrs.Publication], ikz: String): Seq[tigrs.Publication] = publications.filter(_.owner.map(_.ikz).toSeq.flatten contains ikz)

  def pubGraph(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    val authors = publications.flatMap(_.authors).map(a => a.id -> Author(a.id)).toMap
    val pubs = publications.map(p => Publication(p.recordId))
    val outlets = publications.flatMap(_.outlet.map(o => Outlet(o.name))).distinct
    val projects = publications.flatMap(_.projects.map(p => Project(p.id))).distinct
    val keywords = publications.flatMap(_.keywords.map(k => Keyword(k.keyword))).distinct

    val vertices = authors.values ++ pubs ++ outlets ++ projects ++ keywords

    val edges: Seq[Edge[Vertex]] = publications.flatMap { p =>
      p.authors.map(a => Edge[Vertex](authors(a.id), Publication(p.recordId))) ++
        p.outlet.map(o => Edge[Vertex](Outlet(o.name), Publication(p.recordId))) ++
        p.projects.map(pr => Edge[Vertex](Project(pr.id), Publication(p.recordId))) ++
        p.keywords.map(k => Edge[Vertex](Keyword(k.keyword), Publication(p.recordId)))
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def authorGraph(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    val authors = publications.flatMap(_.authors).map(a => a.id -> Author(a.id)).toMap
    val vertices = authors.values

    val edges: Seq[Edge[Vertex]] = publications.flatMap { p =>
      println(p.authors)
      p.authors.combinations(2).map { case Seq(a, b) => Edge[Vertex](authors(a.id), authors(b.id)) }
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def pubCliqueGraphByAuthor(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    val pubs = publications.map(p => Publication(p.recordId))
    val vertices = pubs

    val edges = publications.combinations(2).collect {
      // case Seq(a, b) if (a.authors intersect b.authors).size.toDouble / (a.authors union b.authors).toSet.size > 0.3 =>
      case Seq(a, b) if (a.authors == b.authors) =>
        Edge[Vertex](Publication(a.recordId), Publication(b.recordId))
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def pubCliqueMergedGraphByAuthor(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    val pubMap = publications.map(p => p.recordId -> p).toMap
    val authors = publications.flatMap(_.authors).map(a => a.id -> Author(a.id)).toMap

    val components: Set[Set[tigrs.Publication]] = {
      val vertices = pubMap.values
      val edges = publications.combinations(2).collect {
        // case Seq(a, b) if (a.authors intersect b.authors).size.toDouble / (a.authors union b.authors).toSet.size > 0.4 =>
        case Seq(a, b) if (a.authors == b.authors) =>
          Edge(a, b)
      }
      DirectedGraph(vertices.toSet, edges.toSet).connectedComponents
    }

    val pubSets: Iterable[PublicationSet] = components.map(vs => PublicationSet(vs.map(_.recordId)))
    val vertices: Iterable[Vertex] = pubSets ++ authors.values
    val edges: Iterable[Edge[Vertex]] = pubSets.flatMap(p => pubMap(p.ids.head).authors.map(a => Edge[Vertex](p, Author(a.id))))
    DirectedGraph(vertices.toSet, edges.toSet)
  }
}
