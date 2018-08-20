object P18 {
    def slice[A](l: Int, r: Int, ls: List[A]) = if(l >= r) Nil else ls take r drop l

    //built-in
    def sliceBuiltIn[A](l: Int, r: Int, ls: List[A]) = {
        ls slice (l, r)
    }

    def main(args: Array[String]): Unit = {
        Console println slice(2, 3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        Console println sliceBuiltIn(3, 5, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }

}
