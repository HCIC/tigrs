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

  def pubCliqueMergedGraph(publications: Seq[tigrs.Publication], pubThreshold: Double = 1.0, authorThreshold: Double = 1.0): DirectedGraph[Vertex] = {
    val authors: Set[tigrs.Author] = publications.flatMap(_.authors)(breakOut)
    val aToP: Map[tigrs.Author, Set[tigrs.Publication]] = publications.flatMap(p => p.authors.map(a => a -> p)).groupBy(_._1).mapValues { v => v.map { case (_, p) => p }(breakOut) }

    val pubSets: Iterable[PublicationSet] = publications.groupBy(_.authors.toSet).values.map(ps => PublicationSet(ps.map(_.recordId)(breakOut), ps.toSet))
    val authorSets: Iterable[AuthorSet] = authors.groupBy(aToP).values.map(as => AuthorSet(as.map(_.id)(breakOut), as.toSet))

    val aToAs: Map[tigrs.Author, AuthorSet] = authorSets.flatMap(as => as.as.map(a => a -> as))(breakOut[Iterable[AuthorSet], (tigrs.Author, AuthorSet), Map[tigrs.Author, AuthorSet]])

    val vertices: Set[Vertex] = Set.empty ++ pubSets ++ authorSets
    val edges: Set[Edge[Vertex]] = pubSets.flatMap(ps => ps.ps.flatMap(p => p.authors.map(aToAs))(breakOut[Set[tigrs.Publication], AuthorSet, Set[AuthorSet]]).map((as: AuthorSet) => Edge(ps, as)))(breakOut)
    DirectedGraph(vertices, edges)
  }
}
