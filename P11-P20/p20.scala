object P20 {
    def removeAt[A](n: Int, ls: List[A]) = {
        val tail = ls drop n
        (ls.take(n) ::: tail.tail, tail.head)
    }

    def removeAt1[A](n: Int, ls: List[A]) = {
        val (pre, post) = ls.splitAt(n)
        (pre ::: post.tail, post.head)
    }

    def main(args: Array[String]): Unit = {
        Console println removeAt(1, List('a, 'b, 'c, 'd))
        Console println removeAt1(1, List('a, 'b, 'c, 'd))
    }

}
