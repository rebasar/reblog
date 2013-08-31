package models

import reactivemongo.bson._

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
