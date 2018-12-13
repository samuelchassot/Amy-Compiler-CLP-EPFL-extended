object Foo{

  x match {
    case y => 3
    case x.y(()) => 3
    case Nil() => y match {
      case Nil() => 2
      case True => 2
    }
    case True => 1
    case "Salut" => 3
  }
}