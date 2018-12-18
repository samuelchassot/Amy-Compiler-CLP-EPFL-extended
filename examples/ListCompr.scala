object ListCompr{
  val xs : L.List = L.Cons(1, L.Cons(2, L.Cons(4, L.Nil())));
  val ys : L.List = [ x for x in xs if (x%2 == 0)];
  Std.printString(L.toString(ys))
}
