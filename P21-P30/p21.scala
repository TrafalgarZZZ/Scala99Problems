object P21 {
    def insertAt[A](elem: A, index: Int, ls: List[A]) =
        (ls.take(index) :+ elem) ::: ls.drop(index)

    // provided by website author
    def insertAt1[A](elem: A, index: Int, ls: List[A]) = ls.splitAt(index) match {
        case (pre, post) => pre ::: elem :: post
    }


    def main(args: Array[String]): Unit = {
        Console println insertAt('new, 1, List('a, 'b, 'c, 'd))
        Console println insertAt1('new, 1, List('a, 'b, 'c, 'd))
    }

}
