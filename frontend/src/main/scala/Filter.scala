package tigrs

import pharg._

import graph.Vertex

case class Filters(
  keyword: KeywordFilter = KeywordFilter(),
  author: AuthorFilter = AuthorFilter(),
  limit: LimitFilter = LimitFilter()
) {
  val pubFilters: Seq[PublicationFilter] = List(keyword, author, limit)
  val graphFilters: Seq[GraphFilter] = List()
  val filters: Seq[Filter] = pubFilters ++ graphFilters

  def applyPubFilters(publications: Seq[Publication]): Seq[Publication] = {
    println("applying publication filters...")
    val filtered = pubFilters.foldLeft(publications) { (g, f) => println(s" ${f.getClass.getName}"); f(g) }
    filtered
  }

  def applyGraphFilters(graph: DirectedGraph[Vertex]): DirectedGraph[Vertex] = {
    println("applying graph filters...")
    val filtered = graphFilters.foldLeft(graph) { (g, f) => println(s" ${f.getClass.getName}"); f(g) }
    filtered
  }
}

sealed trait Filter
sealed trait GraphFilter extends Filter {
  def apply(graph: DirectedGraph[Vertex]): DirectedGraph[Vertex]
}
sealed trait PublicationFilter extends Filter {
  def apply(publications: Seq[Publication]): Seq[Publication]
}

case class LimitFilter(limit: Int = 1000) extends PublicationFilter {
  def apply(publications: Seq[Publication]): Seq[Publication] = {
    publications.take(limit)
  }
}

case class KeywordFilter(query: String = "") extends PublicationFilter {
  val lowerQuery = query.toLowerCase
  def apply(publications: Seq[Publication]): Seq[Publication] = {
    if (query.isEmpty) return publications
    publications.filter(_.keywords.exists(_.keyword.toLowerCase containsSlice lowerQuery))
  }
}
case class AuthorFilter(query: String = "") extends PublicationFilter {
  val lowerQuery = query.toLowerCase
  def apply(publications: Seq[Publication]): Seq[Publication] = {
    if (query.isEmpty) return publications
    publications.filter(_.authors.exists(_.name.toLowerCase containsSlice lowerQuery))
  }
}

case class MinDegreeFilter(minDegree: Int) extends GraphFilter {
  def apply(graph: DirectedGraph[Vertex]): DirectedGraph[Vertex] = {
    graph filter (graph.degree(_) >= minDegree)
  }
}
