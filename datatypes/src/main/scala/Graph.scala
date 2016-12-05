package tigrs

import pharg.{DirectedGraph, Edge}
import collection.{breakOut, mutable, immutable}

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
    type P = tigrs.Publication
    type A = tigrs.Author

    val authors: Set[tigrs.Author] = publications.flatMap(_.authors)(breakOut)

    val aToP: Map[A, Set[P]] = {
      val map = mutable.HashMap.empty[A, mutable.HashSet[P]].withDefaultValue(mutable.HashSet.empty)
      for (publication <- publications; author <- publication.authors)
        map(author) = map(author) + publication
      map.mapValues(_.toSet).toMap
    }

    val authorSetSimilarityMatrix: Map[Set[P], Int] = {
      val matrix = mutable.HashMap.empty[Set[P], Int].withDefaultValue(0)
      for ((_, ps) <- aToP; Seq(pa, pb) <- ps.toSeq.combinations(2)) {
        matrix(Set(pa, pb)) += 1
      }
      matrix.toMap
    }

    val publicationSetSimilarityMatrix: Map[Set[A], Int] = {
      val matrix = mutable.HashMap.empty[Set[A], Int].withDefaultValue(0)
      for (p <- publications; Seq(aa, ab) <- p.authors.toSeq.combinations(2)) {
        matrix(Set(aa, ab)) += 1
      }
      matrix.toMap
    }

    val mergablePublications: Set[Edge[P]] = authorSetSimilarityMatrix.collect {
      case (pubPair, commonAuthors) if commonAuthors.toDouble / (pubPair.head.authors.size max pubPair.last.authors.size) >= authorThreshold =>
        Edge(pubPair.head, pubPair.last)
    }(breakOut)
    val mergableAuthors: Set[Edge[A]] = publicationSetSimilarityMatrix.collect {
      case (authorPair, commonPubs) if commonPubs.toDouble / (aToP(authorPair.head).size max aToP(authorPair.last).size) >= pubThreshold =>
        Edge(authorPair.head, authorPair.last)
    }(breakOut)
    val pubSets: Set[PublicationSet] = DirectedGraph(vertices = publications.toSet, edges = mergablePublications).connectedComponents.map(ps => PublicationSet(ps.map(_.recordId), ps))
    val authorSets: Set[AuthorSet] = DirectedGraph(vertices = authors, edges = mergableAuthors).connectedComponents.map(as => AuthorSet(as.map(_.id), as))

    val vertices: Set[Vertex] = pubSets ++ authorSets
    val aToAs: Map[A, AuthorSet] = authorSets.flatMap(as => as.as.map(a => a -> as))(breakOut)
    val edges: Set[Edge[Vertex]] = pubSets.flatMap(ps => ps.ps.flatMap(p => p.authors.map(aToAs))(breakOut[Set[P], AuthorSet, Set[AuthorSet]]).map((as: AuthorSet) => Edge(ps, as)))(breakOut)
    DirectedGraph(vertices, edges)
  }
}
