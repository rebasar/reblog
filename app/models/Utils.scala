package models

import reactivemongo.bson._
import laika.api.Transform
import laika.io.Input.fromString
import laika.parse.markdown.Markdown
import laika.parse.rst.ReStructuredText
import laika.render.HTML

object QueryBuilder {

  def fromParameters(params : URLParameters) = {
    val base = BSONDocument()
    val tagFiltered = if (params.tags.isEmpty) {
      base
    } else {
      base.add(BSONDocument("tags" -> BSONDocument("$in" -> params.tags)))
    }
    params.language match {
      case None => tagFiltered
      case Some(lang) => tagFiltered.add(BSONDocument("language" -> lang.languageCode))
    }
  }

  def fromSlug(slug : String) = BSONDocument("slug" -> slug)

}

object ContentFormatter {

  def toHTML(format : ArticleFormat, content : String) : String = {
    val transform = buildTransform(format)
    transform(content)
  }

  def buildTransform(format : ArticleFormat) = format match {
    case HTMLFormat => identity[String]_
    case RSTFormat => { content : String => Transform from ReStructuredText to HTML fromString content toString }
    case MarkdownFormat => { content : String => Transform from Markdown to HTML fromString content toString }
  }

}

