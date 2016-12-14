package tigrs

import pharg.{DirectedGraph, Edge}
import collection.{breakOut, mutable, immutable}

package graph {
  sealed trait Vertex
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
}

package object graph {
  def filterByIkz(publications: Seq[tigrs.Publication], ikz: String): Seq[tigrs.Publication] = publications.filter(_.owner.map(_.ikz).toSeq.flatten contains ikz)

  def time[T](name: String)(code: => T): T = {
    val start = System.nanoTime
    val result: T = code
    val duration = (System.nanoTime - start) / 1000000
    println(s"$name: ${duration}ms")
    result
  }

  def mergedGraph(pubThreshold: Double, authorThreshold: Double)(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    type P = tigrs.Publication
    type A = tigrs.Author

    val authors: Set[tigrs.Author] = /*time("authors")*/ { publications.flatMap(_.authors)(breakOut) }

    val aToP: collection.Map[A, collection.Set[P]] = /*time("aToP")*/ {
      val map = new mutable.HashMap[A, Set[P]]()
      for (publication <- publications; author <- publication.authors)
        map(author) = map.getOrElse(author, Set.empty) + publication
      map
    }

    val authorSetSimilarityMatrix: collection.Map[(P, P), Int] = /*time("authorMatrix")*/ {
      val matrix = new mutable.HashMap[(P, P), Int]()
      // println("combinations: " + aToP.map { case (_, ps) => ps.toSeq.combinations(2).size }.toSeq.sorted)
      for ((_, ps) <- aToP; pa <- ps; pb <- ps if pa.recordId > pb.recordId) {
        val pair = (pb, pa)
        matrix(pair) = matrix.getOrElse(pair, 0) + 1
      }
      matrix
    }

    val publicationSetSimilarityMatrix: collection.Map[(A, A), Int] = /*time("pubMatrix")*/ {
      val matrix = new mutable.HashMap[(A, A), Int]()
      for (p <- publications; aa <- p.authors; ab <- p.authors if aa.id > ab.id) {
        val pair = (ab, aa)
        matrix(pair) = matrix.getOrElse(pair, 0) + 1
      }
      matrix
    }

    val mergablePublications: Set[Edge[P]] = /*time("mergePubEdges")*/ {
      authorSetSimilarityMatrix.collect {
        case ((pa, pb), commonAuthors) if commonAuthors.toDouble / (pa.authors.size max pb.authors.size) >= authorThreshold =>
          Edge(pa, pb)
      }(breakOut)
    }
    val mergableAuthors: Set[Edge[A]] = /*time("mergeAuthorEdges")*/ {
      publicationSetSimilarityMatrix.collect {
        case ((aa, ab), commonPubs) if commonPubs.toDouble / (aToP(aa).size max aToP(ab).size) >= pubThreshold =>
          Edge(aa, ab)
      }(breakOut)
    }
    val pubSets: Set[PublicationSet] = /*time("cc pubsets")*/ { DirectedGraph(vertices = publications.toSet, edges = mergablePublications).connectedComponents.map(ps => PublicationSet(ps.map(_.recordId), ps)) }
    val authorSets: Set[AuthorSet] = /*time("cc authorsets")*/ { DirectedGraph(vertices = authors, edges = mergableAuthors).connectedComponents.map(as => AuthorSet(as.map(_.id), as)) }

    val vertices: Set[Vertex] = /*time("vertices")*/ { pubSets ++ authorSets }
    val aToAs: Map[A, AuthorSet] = /*time("aToAs")*/ { authorSets.flatMap(as => as.as.map(a => a -> as))(breakOut) }
    val edges: Set[Edge[Vertex]] = /*time("edges")*/ { pubSets.flatMap(ps => ps.ps.flatMap(p => p.authors.map(aToAs))(breakOut[Set[P], AuthorSet, Set[AuthorSet]]).map((as: AuthorSet) => Edge(ps, as)))(breakOut) }

    DirectedGraph(vertices, edges)
  }
}
