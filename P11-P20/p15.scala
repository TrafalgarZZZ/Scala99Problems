object p15 {
    def duplicateN[A](n: Int, ls: List[A]) = ls flatMap {e => List.fill(n)(e)}

    def main(args: Array[String]): Unit = {
        println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
    }
}
