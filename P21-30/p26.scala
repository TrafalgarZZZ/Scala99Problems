object P26 {
    def combinations[A](n: Int, ls: List[A]) = {
        def combinationsR(n: Int, result: List[A], ls: List[A]): List[List[A]] =
            n match {
                case 0 => List(result)
                case _ => {
                    val len = ls.length
                    val ret = for(i <- 0 to len - n) yield combinationsR(n - 1, result :+ ls(i), ls.drop(i + 1))
                    ret.reduceLeft(_ ::: _)
                }
            }
        combinationsR(n, Nil, ls)
    }

    //solution provided by the website author
    // flatMapSublists is like list.flatMap, but instead of passing each element
    // to the function, it passes successive sublists of L.
    def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
        ls match {
            case Nil => Nil
            case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
        }


    def combinations1[A](n: Int, ls: List[A]): List[List[A]] =
        if(n == 0) List(Nil)
        else flatMapSublists(ls) { sl =>
            //combinations1(n - 1, sl.tail) return the result of subsets with n-1 to be chosen
            //and map to get s1.head connected to every possible combination above
            //so we can get the result of the whole combination with n elems to be chosen
            combinations1(n - 1, sl.tail) map {sl.head :: _}
        }



    def main(args: Array[String]): Unit = {
        Console println combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
        Console println combinations(1, List('a, 'b, 'c, 'd, 'e, 'f))
        //C(6, 3) = 20
        Console println combinations1(3, List('a, 'b, 'c, 'd, 'e, 'f)).size
    }

}
