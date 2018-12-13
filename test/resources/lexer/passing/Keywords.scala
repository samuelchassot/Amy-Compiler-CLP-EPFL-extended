abstract Boolean
case class def else error extends false if Int match object String
for in
true Unit val


[ expr for id1 in id2 if(cond) ] ==>> []foo1(id2)



def []foo1(x1: L.List) = {

  x1 match {
    case L.Cons(i, tail) => val id1 : Int = i; if(cond) L.Cons(expr, foo(tail)) else foo(tail)
    case L.Nil() => L.Nil()
  }
}