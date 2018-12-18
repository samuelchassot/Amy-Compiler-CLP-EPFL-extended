object ListCompr{
  val xs : L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));
  val ys : L.List = [ 2*x for x in L.concat(xs, xs) if !(x%2 == 0)];
  Std.printString(L.toString(ys))
}
