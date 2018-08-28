import util.Random
object P23 {
    def randomSelect[A](n: Int, ls: List[A]): List[A] = {
        def randomSelectR(n:Int, ls: List[A], r: Random): List[A] =
            if(n <= 0) Nil
            else {
                val (rest, e) = removeAt1(r.nextInt(ls.length), ls)
                e :: randomSelectR(n - 1, ls, r)
            }
        randomSelectR(n, ls, new Random )
    }

    def removeAt1[A](n: Int, ls: List[A]) = {
        val (pre, post) = ls.splitAt(n)
        (pre ::: post.tail, post.head)
    }

    def main(args: Array[String]): Unit = {
        Console println randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    }

}
