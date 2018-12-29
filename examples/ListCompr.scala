object ListCompr{
  def externalFunction(xs: L.List, t: Int, a: Int): L.List = {
    [t*x*a for x in xs if x < t]
  }

  val xs : L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));
  val ys : L.List = [x for x in xs];
  val zs : L.List = [2*x for x in xs if !(x % 2 == 0)];

  val ls : L.List = [ x*y*z for x in xs for y in ys for z in zs];
  val ms : L.List = [ x*y*z for x in xs for y in ys for z in zs if !(x%2 == 0) && y == 1];
  val ns : L.List = [ x*y*z for x in L.concat(xs, xs) for y in xs for z in [2*x for x in xs] if !(x%2 == 0) && z == 2];

  val os : L.List = [ x*y*z for x in xs for y in L.Nil() for z in zs];

  val t: Int = 1;
  val es : L.List = externalFunction(ys, 3, 2);
  val fs: L.List = [x*y for x in xs for y in ys if (x*y <= t)];

  Std.printString(L.toString(xs));
  Std.printString(L.toString(ys));
  Std.printString(L.toString(zs));
  Std.printString(L.toString(ls));
  Std.printString(L.toString(ms));
  Std.printString(L.toString(ns));
  Std.printString(L.toString(os));
  Std.printString(L.toString(es));
  Std.printString(L.toString(fs))
}

