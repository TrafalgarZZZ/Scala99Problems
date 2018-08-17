object P04 {
    def length[A](ls: List[A]): Int = {
        def lengthR(l: List[A]): Int = ls match {
            case Nil => 0
            case _ :: tail => 1 + lengthR(tail)
        }
        lengthR(ls)
    }

    def lengthTailRecursive[A](ls: List[A]): Int = {
        @scala.annotation.tailrec
        def lengthR(result: Int, curList: List[A]): Int = curList match {
            case Nil       => result
            case _ :: tail => lengthR(result + 1, tail)
        }
        lengthR(0, ls)
    }


    def main(args: Array[String]): Unit = {
        val ls = List(3, 5, 7, 8, 10)
        Console println lengthTailRecursive(ls)
    }

}
