object ListCompr{
  def externalFunction(xs: L.List, t: Int, a: Int): L.List = {
    [t*x*a for x in xs if x < t]
  }

  val nils : L.List = [x for x in L.Nil()];

  val xs : L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));
  val ys : L.List = [x for x in xs];
  val zs : L.List = [2*x for x in xs if !(x % 2 == 0)];

  val as : L.List = [ x*y*z for x in xs for y in ys for z in zs];
  val bs : L.List = [ x*y*z for x in xs for y in ys for z in zs if !(x%2 == 0) && y == 1];
  val cs : L.List = [ x*y*z for x in L.concat(xs, xs) for y in xs for z in [2*x for x in xs] if !(x%2 == 0) && z == 2];

  val ds : L.List = [ x*y*z for x in xs for y in L.Nil() for z in zs];

  val t: Int = 1;
  val es : L.List = externalFunction(ys, 3, 2);
  val fs: L.List = [x*y for x in xs for y in ys if (x*y <= t)];

  Std.printString("nils : " ++ L.toString(nils));
  Std.printString("xs : " ++ L.toString(xs));
  Std.printString("ys : " ++ L.toString(ys));
  Std.printString("zs : " ++ L.toString(zs));
  Std.printString("as : " ++ L.toString(as));
  Std.printString("bs : " ++ L.toString(bs));
  Std.printString("cs : " ++ L.toString(cs));
  Std.printString("ds : " ++ L.toString(ds));
  Std.printString("es : " ++ L.toString(es));
  Std.printString("fs : " ++ L.toString(fs))
}

