object P15 {
    def duplicateN[A](n: Int, ls: List[A]) = ls flatMap {e => List.fill(n)(e)}

    def duplicateN2[A](n: Int, ls: List[A]): List[A] = ls.flatMap(List.iterate(_, n)(x => x))


    def main(args: Array[String]): Unit = {
        println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
    }
}
