def fun(xs: List): List = {
	case Cons(x, tail) => fun2(ys) :: fun(tail)
}

def fun2(ys : List, x: Int): List = {
	
}


[ x*y*z for x in xs for y in ys for z in zs ] 		==> call funX(xsExpr, ysExpr, zsExpr)

def funX(xs, ys, zs) : List = {
	xs match {
		case Cons(x, tail) => funY(ys, zs, x) ::: funX(tail, ys, zs)
		case Nil() => Nil()
	}
}
def funY(ys, zs, x) : List = {
	ys match {
		case Cons(y, tail) => funZ(zs, x, y) ::: funY(tail, zs, x)
		case Nil() => Nil() 
	}
}
def funZ(zs, x, y) :List  {
	zs match {
		case Cons(z, tail) => if(cond) Cons(expr, funZ(tail, x, y)) else funZ(tail, x, y)
		case Nil() => Nil() 
	}
}

def generateFunction(lists : List[(Id, Id)], newArgs: List[Id]) : List[ClassOrFunDef] = {
	lists match{
		case head :: Nil => funBase(head._2, newArgs)
		case head :: tail => {
			val rests : List[ClassOrFunDef] = generateFunction(tail, head._1 :: newArgs)

			new FunDef(
				def fun1(lists.map(_._2).map(toParamDef), newArgs.map(toParamDef)) : List ={
					xs match{
						case Cons(lists.head._1, tail) => Concat(call rests.head(tail.map(._2) :: lists.head._1 :: newArgs ), fun1(tail, newArgs.map(toParamDef))
					}
			) :: rests

		}
	}
	
	}
}



object main {
	val x = 3

	fun2()

}