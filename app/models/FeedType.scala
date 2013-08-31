package models

import play.api.mvc.PathBindable

sealed abstract class FeedType {
  val name : String
  val contentType : String
  val typeName : String
}

object FeedType {

  def fromName(name : String) = name match {
    case "rss" => RSS2
    case "atom" => Atom
  }

  implicit def pathBinder(implicit stringBinder: PathBindable[String]) = new PathBindable[FeedType] {
    override def bind(key: String, value: String): Either[String, FeedType] = {
        stringBinder.bind(key, value) match {
          case Right(name) => Right(fromName(name))
          case _ => Left("unable to bind feed type")
        }
    }

    override def unbind(key: String, feedType: FeedType): String = {
      stringBinder.unbind(key, feedType.name)
    }
  }
}

case object RSS2 extends FeedType {

  override val name = "rss"
  override val contentType = "application/rss+xml"
  override val typeName = "rss_2.0"

}

case object Atom extends FeedType {

  override val name = "atom"
  override val contentType = "application/atom+xml"
  override val typeName = "atom_1.0"

}
