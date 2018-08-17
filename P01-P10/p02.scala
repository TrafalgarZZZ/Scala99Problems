object P02 {
    def penultimate[A](ls: List[A]): A = ls match{
        case a :: _ :: Nil => a
        case _ :: tail => penultimate(tail)
        case _ => throw new NoSuchElementException
    }

    def lastNth[A](ls: List[A], n: Int): A =
        if(n == 1) ls.last
        else    lastNth(ls.init, n - 1)

    def main(args: Array[String]): Unit = {
        val ls = List(3, 5, 7)
        println(penultimate(ls))
    }



}
