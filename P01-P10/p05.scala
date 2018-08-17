object P05 {
    def reverse[A](ls: List[A]): List[A] = ls.reverse
    def reverseRecursive[A](ls: List[A]): List[A] = {
        def reverseR(ls: List[A], resultList: List[A]): List[A] = ls match {
            case Nil => resultList
            case h :: tail => reverseR(tail, h :: resultList )
        }
        reverseR(ls, Nil)
    }

    def reversePureFunctional[A](ls: List[A]): List[A] = {
        ls.foldLeft(List[A]()){(a, b) => b :: a}
    }

    def main(args: Array[String]): Unit = {
        val ls = List(3, 4, 5, 7, 10)
        val lss = List()
        val lsss = "sdadas" :: lss
        println(lsss)

    }


}
