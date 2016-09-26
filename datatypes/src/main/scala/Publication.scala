package tigrs

import scala.scalajs.js.annotation._

final case class Institute(ikz: Seq[String])
final case class Project(id: String, name: String)
final case class Keyword(keyword: String)

@JSExportAll
final case class Publication(
  title: String,
  authors: Seq[Author],
  keywords: Seq[Keyword],
  outlet: Option[Outlet],
  origin: Origin,
  uri: Option[String],
  recordId: Int,
  owner: Option[Institute],
  projects: Seq[Project]
) {
  // override def hashCode = recordId.hashCode
}

final case class Origin(date: String, publisher: Option[String])

//TODO: role
final case class Author(id: String, name: String) {
  // override def hashCode = id.hashCode
}

sealed trait Outlet {
  def name: String
}

object PublicationPickler {
  import boopickle.Default._
  implicit val outletPickler = compositePickler[Outlet].
    addConcreteType[Conference].
    addConcreteType[Journal].
    addConcreteType[Series]
}

final case class Conference(name: String) extends Outlet
final case class Journal(name: String) extends Outlet
final case class Series(name: String) extends Outlet
