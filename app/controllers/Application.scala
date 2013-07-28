package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.modules.reactivemongo._
import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.core.commands._
import reactivemongo.api.collections.default.BSONCollection
import models._
import models.JsonFormats._
import models.BSONFormats._
import scala.util._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class Tag(name : String, count : Int, selected : Boolean, href : String)

object Tag {

  def fromBSONDocument(doc : BSONDocument, selectedTags : Set[String]) : Tag = {
    val name = doc.getAs[String]("_id").get
    val count = doc.getAs[Int]("count").get
    val afterClick : Set[String] = if(selectedTags contains name) { selectedTags - name } else { selectedTags + name }
    val href = routes.Application.index(afterClick.toList, None).url
    Tag(name, count, selectedTags.contains(name), href)
  }

}

object Application extends Controller with MongoController {

  def collection: BSONCollection = db[BSONCollection]("entries")

  implicit def tags(selectedTags : Set[String]) = {
    val aggregateCommand = Aggregate("entries", Seq(Unwind("tags"), GroupField("tags")("count" -> SumValue(1)), Sort(Seq(Ascending("_id")))))
    val allTags = db.command(aggregateCommand)
    Await.result(allTags, Duration(1000, "millis")).map(x => Tag.fromBSONDocument(x, selectedTags)).toList
  }

  implicit def languages(language : Option[Language]) = language match {
    case None => Language.all
    case Some(lang) => Language.select(lang)
  }

  def index(tag : List[String], language: Option[Language]) = Action {
    Async {
      val tagSet = tag.toSet
      val query = BSONDocument(
        "$orderby" -> BSONDocument("updateDate" -> -1),
        "$query" -> Article.queryForTags(tagSet))
      val cursor : Cursor[Article] = collection.find(query).cursor[Article]
      cursor.toList.map { result =>
        Ok(views.html.index(result, tagSet, language))
      }
    }
  }

  def page(slug : String) = Action {
    Async {
      val query = BSONDocument(
        "$orderby" -> BSONDocument("updateDate" -> -1),
        "$query" -> Article.queryForSlug(slug))
      val cursor = collection.find(query).one[Article]
      cursor.map { result =>
        result.map {article => Ok(views.html.page(article))}.getOrElse(NotFound)
      }
    }
  }
}
