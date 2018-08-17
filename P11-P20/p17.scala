object P17 {
    def split[A](n: Int, ls: List[A]) = {
        val lss = ls.zipWithIndex span {_._2 != n}
        (lss._1 map {_._1}, lss._2 map {_._1})
    }

    def split1[A](n: Int, ls: List[A]) = (ls.take(n), ls.drop(n))

    //builtin
    def splitBuiltin[A](n: Int, ls: List[A]) = ls.splitAt(n)

    def splitTailRec[A](n: Int, ls: List[A]) = {
        @scala.annotation.tailrec
        def splitR(num: Int, left: List[A], right: List[A]): (List[A], List[A]) =
            (num, right) match {
                case (_, Nil) => (left, Nil)
                case (0, list) => (left, right)
                case (n, h :: tail) => splitR(n - 1, left :+ h, tail)
            }
        splitR(n, Nil, ls)
    }

    def main(args: Array[String]): Unit = {
        Console println split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        Console println split1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        Console println splitTailRec(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

    }

}
