package models

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

case class Article(
  title: String,
  slug: String,
  content: String,
  creationDate: Option[DateTime],
  updateDate: Option[DateTime],
  tags: Set[String],
  language: Language,
  format: ArticleFormat
) {

  def formatReadableDate(dt : DateTime) = {
    val formatter = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm z")
    formatter.print(dt)
  }

  def url : String = controllers.routes.Application.page(slug).url

  def formattedCreationDate = creationDate.map(formatReadableDate(_))
  def formattedUpdateDate = updateDate.map(formatReadableDate(_))

  def toHTML = ContentFormatter.toHTML(format, content)

}
