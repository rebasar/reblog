package models

case class URLParameters(tags : Set[String], language : Option[Language], page : Int) {

  def toPage(page : Int) = URLParameters(tags, language, page)

  def url = controllers.routes.Application.index(tags.toList, language, page)

}
