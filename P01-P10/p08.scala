
object P08 {
    def compress[A](ls: List[A]): List[A] = {
        def compressR(ls: List[A], resultList: List[A]): List[A] = ls match {
            case Nil => resultList
            case b :: Nil => resultList :+ b
            case a :: b :: _ if a != b => compressR(ls.tail, resultList :+ a)
            case _ => compressR(ls.tail, resultList)
        }
        compressR(ls, Nil)
    }

    def compressFunctional[A](ls: List[A]): List[A] = {
        ls.foldLeft(List[A]()){(r, h) =>
            if(r.isEmpty || r.last == h) r :+ h
            else r
        }
    }

    def compressFunctional2[A](ls: List[A]): List[A] = ls match {
        case Nil => Nil
        case h :: tail => h :: compress(tail.dropWhile(_ == h))
    }


    def main(args: Array[String]): Unit = {
        val ls: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
        println(compress(ls))
        println(compressFunctional2(ls))
    }

}
