package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.modules.reactivemongo._
import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.api.collections.default._
import reactivemongo.core.commands._
//import reactivemongo.api.collections.default.BSONCollection
import models._
import models.JsonFormats._
import models.BSONFormats._
import scala.util._
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller with MongoController {

  def collection: BSONCollection = db[BSONCollection]("entries")

  def tags(params : URLParameters) = {
    val aggregateCommand = Aggregate("entries", Seq(Unwind("tags"), GroupField("tags")("count" -> SumValue(1)), Sort(Seq(Ascending("_id")))))
    val allTags = db.command(aggregateCommand)
    allTags.map { x => x.map(Tag.fromBSONDocument(_, params)).toList }
    // Await.result(allTags, Duration(1000, "millis")).map(x => Tag.fromBSONDocument(x, params)).toList
  }

  implicit def languages(params : URLParameters) = params.language match {
    case None => Language.all(params)
    case Some(lang) => Language.select(lang, params)
  }

  def index(tag : List[String], language: Option[Language], page: Int) = Action.async {implicit request: Request[_] =>
    val tagSet = tag.toSet
    val params = URLParameters(tagSet, language, page)
    val query = BSONDocument(
      "$orderby" -> BSONDocument("updateDate" -> -1),
      "$query" -> QueryBuilder.fromParameters(params))
    val countCommand = Count("entries", Some(QueryBuilder.fromParameters(params)))
    val countResult = db.command(countCommand)
    val cursor : Cursor[Article] = collection.find(query).options(QueryOpts(page*10, 0, 0)).cursor[Article]
    countResult.flatMap{ total =>
      tags(params).flatMap { implicit tags =>
        cursor.collect[List](10).map { result =>
          val pagination = Pagination(page, total)
          Ok(views.html.index(result, params, pagination))
        }
      }
    }
  }

  def page(slug : String) = Action.async {
    val query = BSONDocument(
      "$orderby" -> BSONDocument("updateDate" -> -1),
      "$query" -> QueryBuilder.fromSlug(slug))
    val cursor = collection.find(query).one[Article]
    cursor.flatMap(getArticleWithTags(_))
  }

  private def getArticleWithTags(result : Option[Article]) = {
        result.map { article => tags(URLParameters.fromArticle(article)).map { implicit tags =>
            Ok(views.html.page(article))
          }
        }.getOrElse(future(NotFound))
  }
}
