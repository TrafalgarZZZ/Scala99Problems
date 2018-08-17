object P11 {
    def pack[A](ls: List[A]): List[List[A]] = {
        if(ls.isEmpty) List(List())
        else {
            val (packed, next) = ls.span(_ == ls.head)
            if(next == Nil) List(packed)
            else packed :: pack(next)
        }
    }

    def encode[A](ls: List[A]) = {
        for(x <- pack(ls)) yield if(x.length == 1) x.head else (x.length, x.head)
    }

    def main(args: Array[String]): Unit = {
        println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    }

}
