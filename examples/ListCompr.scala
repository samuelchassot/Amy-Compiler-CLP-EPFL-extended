object ListCompr{
  def externalFunction(xs: L.List, t: Int): L.List = {
    [2*x for x in L.concat(xs, xs) if x < t]
  }

  val xs : L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));
  val ys : L.List = [ 2*x for x in L.concat(xs, xs) if !(x%2 == 0)];
  val zs : L.List = externalFunction(ys, 4);
  Std.printString(L.toString(ys));
  Std.printString(L.toString(zs))
}

