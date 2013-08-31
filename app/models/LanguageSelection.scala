package models

case class LanguageSelection(code : String, name : String, selected : Boolean, href : String)

object LanguageSelection {
  def unselected(language : Language, params : URLParameters) = LanguageSelection(language.shortCode, language.name, false, urlForSelectingLanguage(language, params).url)
  def selected(language : Language, params : URLParameters) = LanguageSelection(language.shortCode, language.name, true, urlForSelectingLanguage(language, params).url)

  def urlForSelectingLanguage(language : Language, params : URLParameters) = controllers.routes.Application.index(params.tags.toList, Some(language))

}
