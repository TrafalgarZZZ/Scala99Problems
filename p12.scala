object P12 {
    def decode[B](ls: List[(Int, B)]): List[B] = {
        def decodeR(ans: List[B], ls: List[(Int, B)]): List[B] = ls match{
            case head :: _ => {
                val temp = for(i <- 1 to head._1) yield head._2
                decodeR(ans ::: temp.toList, ls.tail)
            }
            case Nil => ans
        }
        decodeR(Nil, ls)
    }

    //a shorter version
    def decode2[B](ls: List[(Int, B)]): List[B] = {
        ls flatMap {e => List.fill(e._1)(e._2)}
    }

    def main(args: Array[String]): Unit = {
        println(decode2(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
    }
}
