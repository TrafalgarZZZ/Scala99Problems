object P22 {
    def rangeBuiltin(l: Int, r: Int) = List.range(l, r + 1)

    def rangeRecur(l: Int, r: Int): List[Int] = {
        if(l > r) Nil else l :: rangeRecur(l + 1, r)
    }

    //provided by website author
    // The classic functional approach would be to use `unfoldr`, which Scala
    // doesn't have.  So we'll write one and then use it.
    def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
        f(s) match {
            case None => Nil
            case Some((r, n)) => r :: unfoldRight(n)(f)
        }

    def rangeFunctional(start: Int, end: Int): List[Int] =
        unfoldRight(start) { n =>
            if(n > end) None
            else Some((n, n + 1))
        }


    def main(args: Array[String]): Unit = {
        Console println rangeBuiltin(4, 9)
        Console println rangeRecur(4, 9)
        Console println rangeFunctional(4, 9)
    }

}
