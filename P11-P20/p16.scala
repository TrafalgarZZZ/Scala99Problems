object P16{
    def drop[A](n: Int, ls: List[A]): List[A] =
        ls match {
            case Nil => Nil
            case _ => ls.dropRight(ls.length - n + 1) ::: drop(n, ls.drop(n))
        }

    //solution provided by the website author
    def dropFunctional[A](n: Int, ls: List[A]): List[A] =
        ls.zipWithIndex filter {v => (v._2 + 1) % n != 0} map {_._1}

    def main(args: Array[String]): Unit = {
      Console println drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      Console println dropFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }
}
