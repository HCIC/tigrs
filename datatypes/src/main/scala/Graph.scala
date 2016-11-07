package tigrs

import pharg.{DirectedGraph, Edge}
import collection.breakOut
import collection.mutable
import Math._

case class PublicationSeq(ps: Seq[Publication])
case class AuthorSeq(as: Seq[Author])

package graph {
  sealed trait Vertex
  case class Author(id: String) extends Vertex
  case class Publication(id: Int) extends Vertex
  case class PublicationSet(ids: Set[Int]) extends Vertex
  case class AuthorSet(ids: Set[String]) extends Vertex
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

  def pubCliqueGraphByAuthor(publications: Seq[tigrs.Publication], threshold: Double = 1.0): DirectedGraph[tigrs.Publication] = {
    val vertices = publications

    val edges = publications.combinations(2).collect {
      case Seq(a, b) if (a.authors intersect b.authors).size.toDouble / (a.authors union b.authors).distinct.size >= threshold =>
        Edge[tigrs.Publication](a, b)
    }
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def authorCliqueGraphByPublication(publications: Seq[tigrs.Publication], threshold: Double = 1.0): DirectedGraph[graph.Author] = {

    val authors = publications.flatMap(_.authors.map(a => graph.Author(a.id))).distinct
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
    // val authors: Map[String, graph.Author] = publications.flatMap(_.authors).map(a => a.id -> Author(a.id)).toMap

    val pubComponents: Set[Set[tigrs.Publication]] = {
      pubCliqueGraphByAuthor(publications, pubThreshold).connectedComponents
    }
    val authorComponents: Set[Set[graph.Author]] = {
      authorCliqueGraphByPublication(publications, authorThreshold).connectedComponents
    }

    val pubSets: Iterable[PublicationSet] = pubComponents.map(vs => PublicationSet(vs.map(_.recordId)))
    val authorSets: Iterable[AuthorSet] = authorComponents.map(vs => AuthorSet(vs.map(_.id)))

    val vertices: Iterable[Vertex] = pubSets ++ authorSets
    val edges: Iterable[Edge[Vertex]] = for (
      as <- authorSets;
      ps <- pubSets;
      if (ps.ids.flatMap(id => pubMap(id).authors.map(_.id)) intersect as.ids).nonEmpty
    ) yield Edge(ps, as)
    DirectedGraph(vertices.toSet, edges.toSet)
  }

  def fullIntersectionGraph(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    val mergedCliques = pubCliqueMergedGraph(publications)
    val authors: Seq[AuthorSet] = mergedCliques.vertices.collect { case a: AuthorSet => a }(breakOut[Set[Vertex], AuthorSet, List[AuthorSet]])
    val sets: List[(AuthorSet, Set[PublicationSet])] = authors.map { a => a -> mergedCliques.neighbours(a).collect { case p: PublicationSet => p } }.toList
    val zones: List[(AuthorSet, Set[PublicationSet])] = sets.combinations(2).collect {
      case List((authorsA, pubsA), (authorsB, pubsB)) if pubsA exists pubsB =>
        AuthorSet(authorsA.ids union authorsB.ids) -> (pubsA intersect pubsB)
    }.toList
    val vertices: Set[Vertex] = zones.map(_._1)(breakOut[List[(AuthorSet, Set[PublicationSet])], Vertex, Set[Vertex]])
    val edges: Set[Edge[Vertex]] = zones.combinations(2).collect {
      case List((authorsA, pubsA), (authorsB, pubsB)) if authorsA.ids exists authorsB.ids =>
        Edge[Vertex](authorsA, authorsB)
    }.toSet //(breakOut[List[(AuthorSet, PublicationSet)], Edge[AuthorSet], Set[Edge[AuthorSet]]])
    DirectedGraph[Vertex](vertices, edges)
  }

  def intersectionGraph(publications: Seq[tigrs.Publication]): DirectedGraph[Vertex] = {
    // from
    // An Heuristic for the Construction of Intersection Graphs
    // Paolo Simonetto, David Auber

    def full = fullIntersectionGraph(publications)
    val vertices = full.vertices
    val edgePool = mutable.HashSet.empty ++ full.edges
    val edges = mutable.ArrayBuffer.empty[Edge[Vertex]]

    def metric(e: Edge[Vertex]): Double = e match {
      case Edge(a: AuthorSet, b: AuthorSet) =>
        def c(a: AuthorSet, b: AuthorSet): Int = {
          val g = DirectedGraph(vertices, edges.toSet)
          if (g.reachable(a, b))
            a.ids count b.ids
          else
            0
        }
        def u(a: AuthorSet, b: AuthorSet): Double = min(a.ids.size, b.ids.size) - (a.ids count b.ids)
        def v(a: AuthorSet, b: AuthorSet): Double = max(a.ids.size, b.ids.size) - (a.ids count b.ids) - 1
        val p1 = 0.05
        val p2 = 0.01
        c(a, b) - p1 * u(a, b) - p2 * v(a, b)
    }

    while (edgePool.nonEmpty) {
      val best = edgePool maxBy metric
      println(edgePool.size)

      if (DirectedGraph(vertices, edges.toSet + best).isPlanar) edges += best

      edgePool -= best
    }

    DirectedGraph(vertices, edges.toSet)
  }
}
