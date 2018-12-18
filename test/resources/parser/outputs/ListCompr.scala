object ListCompr {
  def ++listComprDesuggar1(xs: L.List): L.List = {
    xs match {
      case L.Cons(x, tail) =>
        (if(true) {
          L.Cons((2 * x), ++listComprDesuggar1(tail))
        } else {
          ++listComprDesuggar1(tail)
        })
      case L.Nil() =>
        L.Nil()
    }
  }
  ListCompr.++listComprDesuggar1(xs)
}
