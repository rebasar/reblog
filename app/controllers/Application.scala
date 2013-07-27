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

case class Tag(name : String, count : Int, selected : Boolean)

object Application extends Controller with MongoController {

  def collection: BSONCollection = db[BSONCollection]("entries")

  implicit def tags(tagList : List[String]) = {
    val aggregateCommand = Aggregate("entries", Seq(Unwind("tags"), GroupField("tags")("count" -> SumValue(1)), Sort(Seq(Ascending("_id")))))
    val allTags = db.command(aggregateCommand)
    Await.result(allTags, Duration(1000, "millis")).map(x => Tag(x.getAs[String]("_id").get, x.getAs[Int]("count").get, tagList.contains(x.getAs[String]("_id").get))).toList
  }

  implicit def languages(language : Option[Language]) = language match {
    case None => Language.all
    case Some(lang) => Language.select(lang)
  }

  def index(tag : List[String], language: Option[Language]) = Action {
    Async {
      val query = BSONDocument(
        "$orderby" -> BSONDocument("updateDate" -> -1),
        "$query" -> Article.queryForTags(tag))
      val cursor : Cursor[Article] = collection.find(query).cursor[Article]
      cursor.toList.map { result =>
        Ok(views.html.index(result, tag, language))
      }
    }
  }
}
