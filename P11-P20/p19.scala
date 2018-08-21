object P19 {
    def rotate[A](n: Int, ls: List[A]) = {
        if(n >= 0){
            ls.drop(n % ls.length) ::: ls.take(n % ls.length)
        }
        else {
            val nx = ls.length + n % ls.length
            ls.drop(nx) ::: ls.take(nx)
        }
    }
    //It's a similar solution provided by the website author
    def rotateO[A](n: Int, ls: List[A]): List[A] = {
        val nBound = if (ls.isEmpty) 0 else n % ls.length
        if(nBound < 0) rotate(nBound + ls.length, ls)
        else (ls drop nBound) ::: (ls take nBound)
    }


    def main(args: Array[String]): Unit = {
        Console println rotate(25, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        Console println rotateO(-24, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }

}
