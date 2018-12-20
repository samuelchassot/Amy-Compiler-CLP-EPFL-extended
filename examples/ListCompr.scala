object ListCompr{
  /*def externalFunction(xs: L.List, t: Int): L.List = {
    [2*x for x in L.concat(xs, xs) if x < t]
  }*/

  val xs : L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));
  val ys : L.List = [ y*x*z for x in L.concat(xs, xs) for y in xs for z in [a for a in xs] if !(x%2 == 0) && z == 1];
  //val zs : L.List = externalFunction(ys, 5);
  val as : L.List = [z for z in ys];
  Std.printString(L.toString(ys));
  //Std.printString(L.toString(zs));
  Std.printString(L.toString(as))
}

