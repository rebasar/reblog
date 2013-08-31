package models

import org.joda.time.DateTime

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
