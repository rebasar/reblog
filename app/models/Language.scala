package models

import play.api.mvc.QueryStringBindable

sealed abstract class Language {
  val shortCode: String
  val languageCode: String
  val name: String
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
