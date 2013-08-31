package models

case class Pagination(previous : Option[Int], current : Int, next : Option[Int], total : Int)

object Pagination {

  def apply(current : Int, total : Int) = {
    val pagecount = total/10
    val previous = if(current == 0) { None } else { Some(current - 1)}
    val next = if(current == pagecount){ None } else { Some(current + 1) }
    new Pagination(previous, current, next, pagecount)
  }

  def appy(previous : Option[Int], current : Int, next : Option[Int], total : Int) = new Pagination(previous, current, next, total)

}
