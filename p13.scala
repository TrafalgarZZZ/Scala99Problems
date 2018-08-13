object P13 {
    def encodeDirect[A](ls: List[A]) = {
        def encodeDirectR(ls: List[A], counter: Int, ans: List[(Int, A)]): List[(Int, A)] = ls match {
            case a :: b :: _ if a == b => encodeDirectR(ls.tail, counter + 1, ans)
            case a :: b :: _ if a != b => encodeDirectR(ls.tail, 0, ans :+ (counter + 1, ls.head))
            case _ :: Nil => encodeDirectR(ls.tail, 0, ans :+ (counter + 1, ls.head))
            case Nil => ans
        }
        encodeDirectR(ls, 0, Nil)
    }

    //a shorter version
    def encodeDirect2[A](ls: List[A]): List[(Int, A)] =
        if (ls.isEmpty) Nil
        else {
            val (packed, next) = ls span {_ == ls.head}
            (packed.length, packed.head) :: encodeDirect(next)
        }

    def main(args: Array[String]): Unit = {
        println(encodeDirect2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    }

}
