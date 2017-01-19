package tigrs

import scala.scalajs.js.annotation._

final case class Institute(ikz: Seq[String])
final case class Project(id: String, name: String)
final case class Keyword(keyword: String)

final case class Publication(
  recordId: Int,
  title: String,
  origin: Origin,
  authors: Set[Author],
  owner: Option[Institute]
// projects: Seq[Project]
// outlet: Option[Outlet],
// keywords: Seq[Keyword],
// uri: Option[String],
) {
  def canEqual(a: Any) = a.isInstanceOf[Publication]

  override def equals(that: Any): Boolean =
    that match {
      case that: Publication => that.canEqual(this) && this.recordId == that.recordId
      case _ => false
    }

  override def hashCode = recordId.hashCode
}
final case class Origin(year: Int, publisher: Option[String])

//TODO: role
final case class Author(id: String, name: String, termsOfAddress: Int) {
  def canEqual(a: Any) = a.isInstanceOf[Author]

  override def equals(that: Any): Boolean =
    that match {
      case that: Author => that.canEqual(this) && this.id == that.id
      case _ => false
    }

  override def hashCode = id.hashCode
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
