object ListComprOtherModule{
  val us : L.List = L.Cons(1, L.Cons(2, L.Cons(3, L.Nil())));
  val vs : L.List = [ 3*x for x in L.concat(us, us) if !(x%3 == 0)];
  val ws : L.List = [ 3*v for v in vs if (v%3 == 0)];
  Std.printString(L.toString(vs));
  Std.printString(L.toString(ws))
}