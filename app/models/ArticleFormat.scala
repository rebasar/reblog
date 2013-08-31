package models

sealed abstract class ArticleFormat {

  def getName : String

}

object ArticleFormat {

  def getAllFormats = List(HTMLFormat, RSTFormat, MarkdownFormat)

  def fromName(name : String) = getAllFormats.filter(_.getName == name).head

  def toName(format : ArticleFormat) = format.getName

}

case object HTMLFormat extends ArticleFormat {

  override def getName = "html"

}

case object RSTFormat extends ArticleFormat {

  override def getName = "rst"

}

case object MarkdownFormat extends ArticleFormat {

  override def getName = "md"

}
