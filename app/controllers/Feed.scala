package controllers

import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.modules.reactivemongo._
import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.api.collections.default._
import reactivemongo.core.commands._
import scala.collection.JavaConversions._
import com.sun.syndication.feed.synd._
import com.sun.syndication.io.SyndFeedOutput
import models._
import models.BSONFormats._

object FeedProducer {

  def produceFeed(feedType : FeedType, articles : List[Article]) = {
    val feed = createFeed(feedType, articles.map(articleToFeedEntry(_)))
    val output = new SyndFeedOutput()
    output.outputString(feed)
  }

  def articleToFeedEntry(article : Article) = {
    val entry = new SyndEntryImpl()
    val body = new SyndContentImpl()
    entry.setTitle(article.title)
    entry.setLink(article.url)
    entry.setPublishedDate(article.creationDate.get.toDate)
    body.setType("text/html")
    body.setValue(article.toHTML)
    entry.setDescription(body)
    entry
  }

  def createFeed[T >: SyndFeed](feedType : FeedType, content : List[T]) = {
    val feed = new SyndFeedImpl()
    feed.setFeedType(feedType.typeName)
    feed.setTitle("reblog")
    feed.setLink("http://tonguc.name")
    feed.setDescription("rebLog")
    feed.setEntries(content)
    feed
  }
}

object Feed extends Controller with MongoController {

  def collection: BSONCollection = db[BSONCollection]("entries")

  def feed(format : FeedType, tag : List[String], language: Option[Language], page: Int) = Action {
    Async {
      val tagSet = tag.toSet
      val params = URLParameters(tagSet, language, page)
      val query = BSONDocument(
        "$orderby" -> BSONDocument("updateDate" -> -1),
        "$query" -> QueryBuilder.fromParameters(params))
      val cursor : Cursor[Article] = collection.find(query).options(QueryOpts(page*10, 0, 0)).cursor[Article]
      cursor.collect[List](10).map { result =>
        SimpleResult(
          header = ResponseHeader(200, Map(CONTENT_TYPE -> format.contentType)),
          body = Enumerator(FeedProducer.produceFeed(format, result))
        )
      }
    }
  }
}
