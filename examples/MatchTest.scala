object MatchTest {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List
  case class Concat(l1: List, l2: List) extends List

  def head(l: List): Int = {
    l match {
      case Cons(x, _) => x
      case Nil() => error("hi")
    }
  }

//  val l: List = Cons(3, Cons(9, Nil()));
  val l: List = Cons(3, Nil());
  val l2: List = Cons(11, Nil());
  val c: List = Concat(l, l2);

//  val y: Int = l match{
//    case Nil() => 100
//    case Cons(3, t) => t match{
//      case Cons(_, _) => 42
//      case Nil() => 10000
//    }
//    case Cons(5, t) => 6
//  };

val y: Int = c match{
  case Nil() => 100000
  case Concat(_, d) => head(d)
  case listVal => head(listVal)
};

  Std.printInt(y)
}
