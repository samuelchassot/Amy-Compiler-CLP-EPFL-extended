object ListCompr{
  val xs : L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));
  val ys : L.List = [ x for x in xs ];
  Std.printString(L.toString(ys))
}
