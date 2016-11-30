package tigrs

import pharg.{DirectedGraph, Edge}
import collection.breakOut

case class PublicationSeq(ps: Seq[Publication])
case class AuthorSeq(as: Seq[Author])

package graph {
  sealed trait Vertex
  case class Publication(id: Int, p: tigrs.Publication) extends Vertex {
    def canEqual(a: Any) = a.isInstanceOf[Publication]

    override def equals(that: Any): Boolean =
      that match {
        case that: Publication => that.canEqual(this) && this.id == that.id
        case _ => false
      }

    override def hashCode = id.hashCode
  }
  case class Author(id: String, a: tigrs.Author) extends Vertex {
    def canEqual(a: Any) = a.isInstanceOf[Author]

    override def equals(that: Any): Boolean =
      that match {
        case that: Author => that.canEqual(this) && this.id == that.id
        case _ => false
      }

    override def hashCode = id.hashCode
  }

  case class PublicationSet(ids: Set[Int], ps: Set[tigrs.Publication]) extends Vertex {
    def canEqual(a: Any) = a.isInstanceOf[PublicationSet]

    override def equals(that: Any): Boolean =
      that match {
        case that: PublicationSet => that.canEqual(this) && this.ids == that.ids
        case _ => false
      }

    override def hashCode = ids.hashCode
  }
  case class AuthorSet(ids: Set[String], as: Set[tigrs.Author]) extends Vertex {
    def canEqual(a: Any) = a.isInstanceOf[AuthorSet]

    override def equals(that: Any): Boolean =
      that match {
        case that: AuthorSet => that.canEqual(this) && this.ids == that.ids
        case _ => false
      }

    override def hashCode = ids.hashCode
  }
  case class Keyword(keyword: String) extends Vertex
  case class Project(id: String) extends Vertex
  case class Outlet(name: String) extends Vertex
}

package object graph {
  def filterByIkz(publications: Seq[tigrs.Publication], ikz: String): Seq[tigrs.Publication] = publications.filter(_.owner.map(_.ikz).toSeq.flatten contains ikz)

  def pubGraph(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    val authors = publications.flatMap(_.authors).map(a => a.id -> Author(a.id, a)).toMap
    val pubs = publications.map(p => Publication(p.recordId, p))
    val outlets = publications.flatMap(_.outlet.map(o => Outlet(o.name))).distinct
    val projects = publications.flatMap(_.projects.map(p => Project(p.id))).distinct
    val keywords = publications.flatMap(_.keywords.map(k => Keyword(k.keyword))).distinct

    val vertices = authors.values ++ pubs ++ outlets ++ projects ++ keywords

    val edges: Seq[Edge[Vertex]] = publications.flatMap { p =>
      p.authors.map(a => Edge[Vertex](authors(a.id), Publication(p.recordId, p))) ++
        p.outlet.map(o => Edge[Vertex](Outlet(o.name), Publication(p.recordId, p))) ++
        p.projects.map(pr => Edge[Vertex](Project(pr.id), Publication(p.recordId, p))) ++
        p.keywords.map(k => Edge[Vertex](Keyword(k.keyword), Publication(p.recordId, p)))
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def authorGraph(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    val authors = publications.flatMap(_.authors).map(a => a.id -> Author(a.id, a)).toMap
    val vertices = authors.values

    val edges: Seq[Edge[Vertex]] = publications.flatMap { p =>
      println(p.authors)
      p.authors.combinations(2).map { case Seq(a, b) => Edge[Vertex](authors(a.id), authors(b.id)) }
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def pubCliqueGraphByAuthor(publications: Seq[tigrs.Publication], threshold: Double = 1.0): DirectedGraph[tigrs.Publication] = {
    val vertices = publications

    // val authorToPubSet: Map[tigrs.Author, Set[tigrs.Publication]] = publications.flatMap(p => p.authors.map(a => p -> a)).groupBy { case (p, a) => a }.mapValues { tuples => tuples.map { case (p, a) => p }(breakOut[Seq[(tigrs.Publication, tigrs.Author)], tigrs.Publication, Set[tigrs.Publication]]) }

    val edges = publications.combinations(2).collect {
      case Seq(a, b) if (a.authors intersect b.authors).size.toDouble / (a.authors union b.authors).distinct.size >= threshold =>
        Edge[tigrs.Publication](a, b)
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def authorCliqueGraphByPublication(publications: Seq[tigrs.Publication], threshold: Double = 1.0): DirectedGraph[graph.Author] = {

    val authors = publications.flatMap(_.authors.map(a => graph.Author(a.id, a))).distinct
    // authors to publications
    val aToP: Map[String, Set[Int]] = publications.flatMap(p => p.authors.map(a => a.id -> p.recordId)).groupBy(_._1).mapValues { v => v.map { case (_, p) => p }.toSet }

    val vertices = authors
    val edges = authors.combinations(2).collect {
      case Seq(a, b) if (aToP(a.id) intersect aToP(b.id)).size.toDouble / (aToP(a.id) union aToP(b.id)).size >= threshold =>
        Edge[graph.Author](a, b)
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def pubCliqueMergedGraph(publications: Seq[tigrs.Publication], pubThreshold: Double = 1.0, authorThreshold: Double = 1.0): DirectedGraph[Vertex] = {
    val pubMap: Map[Int, tigrs.Publication] = publications.map(p => p.recordId -> p).toMap
    // val authors: Map[String, graph.Author] = publications.flatMap(_.authors).map(a => a.id -> Author(a.id, a)).toMap

    val pubComponents: Set[Set[tigrs.Publication]] = {
      pubCliqueGraphByAuthor(publications, pubThreshold).connectedComponents
    }
    val authorComponents: Set[Set[graph.Author]] = {
      authorCliqueGraphByPublication(publications, authorThreshold).connectedComponents
    }

    val pubSets: Iterable[PublicationSet] = pubComponents.map(vs => PublicationSet(vs.map(_.recordId), vs))
    val authorSets: Iterable[AuthorSet] = authorComponents.map(vs => AuthorSet(vs.map(_.id), vs.map(_.a)))

    val vertices: Iterable[Vertex] = pubSets ++ authorSets
    val edges: Iterable[Edge[Vertex]] = for (
      as <- authorSets;
      ps <- pubSets;
      if (ps.ids.flatMap(id => pubMap(id).authors.map(_.id)) intersect as.ids).nonEmpty
    ) yield Edge(ps, as)
    DirectedGraph(vertices.toSet, edges.toSet)
  }
}
