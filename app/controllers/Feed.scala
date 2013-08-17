import models._
import scala.collection.JavaConversions._
import com.sun.syndication.feed.synd._;

object FeedProducer {

  def produceFeed(feedType : FeedType, articles : List[Article]) = createFeed(feedType, articles.map(articleToFeedEntry(_)))

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
    feed.setFeedType(feedType.name)
    feed.setTitle("reblog")
    feed.setLink("http://tonguc.name")
    feed.setDescription("rebLog")
    feed.setEntries(content)
  }
}

abstract class FeedType {
  val name : String
}

case object RSS2 extends FeedType {

  override val name = "rss_2.0"

}

case object Atom extends FeedType {

  override val name = "atom_1.0"

}
