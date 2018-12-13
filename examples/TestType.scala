object Main{
  /*
  abstract class List
  case class Nil() extends List
  case class Cons() extends List

  def func(h : Int): List = {
    Cons()
  }

  val y: List = Cons();
  val x: Int = 3;
  x;
  func(x)
  */
  abstract class List
  case class Nil() extends List
  case class Cons() extends List
  abstract class Perso
  case class cc(x : Int) extends Perso

  val x: List = Cons();
  val y: Perso = cc(8);

  y match{
    case cc(h) => h
    case _ => "abs"
  }

}