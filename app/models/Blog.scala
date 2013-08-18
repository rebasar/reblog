package models

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.mvc.QueryStringBindable
import play.api.mvc.PathBindable
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

case class Pagination(previous : Option[Int], current : Int, next : Option[Int], total : Int)

object Pagination {

  def apply(current : Int, total : Int) = {
    val pagecount = total/10
    val previous = if(current == 0) { None } else { Some(current - 1)}
    val next = if(current == pagecount){ None } else { Some(current + 1) }
    new Pagination(previous, current, next, pagecount)
  }

  def appy(previous : Option[Int], current : Int, next : Option[Int], total : Int) = new Pagination(previous, current, next, total)

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

case class URLParameters(tags : Set[String], language : Option[Language], page : Int) {

  def toPage(page : Int) = URLParameters(tags, language, page)

  def url = controllers.routes.Application.index(tags.toList, language, page)

}

case class Tag(name : String, count : Int, selected : Boolean, href : String)

object Tag {

  def fromBSONDocument(doc : BSONDocument, params : URLParameters) : Tag = {
    val selectedTags = params.tags
    val name = doc.getAs[String]("_id").get
    val count = doc.getAs[Int]("count").get
    val afterClick : Set[String] = if(selectedTags contains name) { selectedTags - name } else { selectedTags + name }
    val href = controllers.routes.Application.index(afterClick.toList, params.language).url
    Tag(name, count, selectedTags.contains(name), href)
  }

}

sealed abstract class Language {
  val shortCode: String
  val languageCode: String
  val name: String
}

case class LanguageSelection(code : String, name : String, selected : Boolean, href : String)

object LanguageSelection {
  def unselected(language : Language, params : URLParameters) = LanguageSelection(language.shortCode, language.name, false, urlForSelectingLanguage(language, params).url)
  def selected(language : Language, params : URLParameters) = LanguageSelection(language.shortCode, language.name, true, urlForSelectingLanguage(language, params).url)

  def urlForSelectingLanguage(language : Language, params : URLParameters) = controllers.routes.Application.index(params.tags.toList, Some(language))

}

object Language {
  def fromLanguageCode(code : String) = code match {
    case "en_GB" => English
    case "tr_TR" => Turkish
  }

  def fromShortCode(code : String) = code match {
    case "en" => English
    case "tr" => Turkish
  }

  def all(params : URLParameters) = List(Turkish, English).map(LanguageSelection.unselected(_, params))

  def select(lang : Language, params : URLParameters) = for (language <- List(Turkish, English))
    yield if(language == lang){
      LanguageSelection.selected(language, params)
    } else {
      LanguageSelection.unselected(language, params)
    }

  implicit def queryStringBinder(implicit stringBinder: QueryStringBindable[String]) = new QueryStringBindable[Language] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Language]] = {
      for {
        value <- stringBinder.bind(key, params)
      } yield {
        value match {
          case Right(code) => Right(fromShortCode(code))
          case _ => Left("unable to bind language")
        }
      }
    }
    override def unbind(key: String, language: Language): String = {
      key + "=" + language.shortCode
    }
  }
}

case object English extends Language{
  val shortCode = "en"
  val languageCode = "en_GB"
  val name: String = "English"
}

case object Turkish extends Language{
  val shortCode = "tr"
  val languageCode = "tr_TR"
  val name: String = "Türkçe"
}

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

object JsonFormats {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  implicit val languageReads : Reads[Language] = {
    JsPath.read[String].map(languageCode => Language.fromLanguageCode(languageCode))
  }

  implicit val articleFormatReads : Reads[ArticleFormat] = {
    JsPath.read[String].map(name => ArticleFormat.fromName(name))
  }

  implicit val languageWrites : Writes[Language] = Writes[Language] { language => JsString(language.languageCode) }

  implicit val articleFormatWrites : Writes[ArticleFormat] = Writes[ArticleFormat] { format => JsString(format.getName)}

  implicit val languageFormat = Format(languageReads, languageWrites)
  implicit val aritcleFormatFormat = Format(articleFormatReads, articleFormatWrites)
  implicit val articleFormat = Json.format[Article]
}

object BSONFormats {
  import reactivemongo.bson._

  implicit object ArticleBSONReader extends BSONDocumentReader[Article] {
    def read(doc: BSONDocument): Article =
      Article(
        doc.getAs[String]("title").get,
        doc.getAs[String]("slug").get,
        doc.getAs[String]("content").get,
        doc.getAs[BSONDateTime]("creationDate").map(dt => new DateTime(dt.value)),
        doc.getAs[BSONDateTime]("updateDate").map(dt => new DateTime(dt.value)),
        doc.getAs[Set[String]]("tags").get,
        Language.fromLanguageCode(doc.getAs[String]("language").get),
        ArticleFormat.fromName(doc.getAs[String]("format").get)
      )
  }
  implicit object ArticleBSONWriter extends BSONDocumentWriter[Article] {
    def write(article: Article): BSONDocument =
      BSONDocument(
        "title" -> article.title,
        "slug" -> article.slug,
        "content" -> article.content,
        "creationDate" -> article.creationDate.map(date => BSONDateTime(date.getMillis)),
        "updateDate" -> article.updateDate.map(date => BSONDateTime(date.getMillis)),
        "tags" -> article.tags,
        "language" -> article.language.languageCode,
        "format" -> ArticleFormat.toName(article.format)
      )
  }


}
