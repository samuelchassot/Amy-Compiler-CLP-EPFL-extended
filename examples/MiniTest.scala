object MiniTest{
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def head(l: List): Int = {
    l match {
      case Cons(x, Cons(j, t)) => x
      case Nil() => error("head on Nil")
    }
  }
  def tail(l: List): List = {
    l match {
      case Cons(_, t) => t
      case Nil() => error("tail on Nil error")
    }
  }
  Cons(3, Cons(5, Nil()))
}
