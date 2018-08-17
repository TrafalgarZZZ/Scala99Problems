object P10 {
    def pack[A](ls: List[A]): List[List[A]] = {
        if(ls.isEmpty) List(List())
        else {
            val (packed, next) = ls.span(_ == ls.head)
            if(next == Nil) List(packed)
            else packed :: pack(next)
        }
    }

    def encode[A](ls: List[A]) = {
        for(x <- pack(ls)) yield (x.length, x.head)
    }

    //a more functional answer
    def encode2[A](ls: List[A]) = {
        pack(ls) map {e => (e.length, e.head)}
    }

    def main(args: Array[String]): Unit = {
        println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
        println(encode2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    }

}
