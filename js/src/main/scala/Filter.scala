package tigrs

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

sealed trait Filter
sealed trait GraphFilter extends Filter {
  def apply(graph: Graph[PubVertex, DiEdge]): Graph[PubVertex, DiEdge]
}
sealed trait PublicationFilter extends Filter {
  def apply(publications: Publications): Publications
}

case class PublicationLimitFilter(limit: Int) extends PublicationFilter {
  def apply(publications: Publications): Publications = {
    Publications(publications.publications.take(limit))
  }
}

case class PublicationKeywordFilter(query: String) extends PublicationFilter {
  val lowerQuery = query.toLowerCase
  def apply(publications: Publications): Publications = {
    Publications(publications.publications.filter(_.keywords.exists(_.keyword.toLowerCase containsSlice lowerQuery)))
  }
}

case class MinDegreeFilter(minDegree: Int) extends GraphFilter {
  def apply(graph: Graph[PubVertex, DiEdge]): Graph[PubVertex, DiEdge] = {
    graph filter graph.having(node = _.degree >= minDegree)
  }
}
