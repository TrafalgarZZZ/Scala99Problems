import util.Random
object P24 {
    //removeAt1 from P20
    def removeAt1[A](n: Int, ls: List[A]) = {
        val (pre, post) = ls.splitAt(n)
        (pre ::: post.tail, post.head)
    }
    //end P20

    def lotto(n: Int, m: Int) = {
        val ls = 1 to m toList
        val rand = new Random
        def lottoR(n: Int, ls: List[Int]): List[Int] = (n, ls) match {
            case (0, _) => Nil
            case _ => {
                val (rest, elem) = removeAt1(rand.nextInt(ls.length), ls)
                elem :: lottoR(n - 1, rest)
            }
        }
        lottoR(n, ls)
    }

    def main(args: Array[String]): Unit = {
        Console println lotto(6, 49)
    }
}
