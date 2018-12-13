object TestLists {
  val l: L.List = L.Cons(5, L.Cons(-5, L.Cons(-1, L.Cons(0, L.Cons(10, L.Nil())))));
  Std.printString(L.toString(L.concat(L.Cons(1, L.Cons(2, L.Nil())), L.Cons(3, L.Nil()))));
  Std.printInt(L.sum(l));
  Std.printString(L.toString(L.mergeSort(l)))


//
//  val p: L.LPair = L.split(l);
//  p match {
//    case L.LP(l1, l2) => Std.printString(L.toString(l1)); Std.printString(L.toString(l2))
//  }
//  val sorted: L.List = L.mergeSort(l);
//  3
}