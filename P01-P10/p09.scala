object P09 {
    def pack[A](ls: List[A]): List[List[A]] = {
        if(ls.isEmpty) List(List())
        else {
            val (packed, next) = ls.span(_ == ls.head)
            if(next == Nil) List(packed)
            else packed :: pack(next)
        }
    }

    def pack2[A](ls: List[A]): List[List[A]] = ls match {
        case Nil => Nil
        case h :: tail => {
            val (toPack, next) = tail.span(_ == h)
            (h :: toPack) :: pack(next)
        }
    }

    def main(args: Array[String]): Unit = {
        println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
        println(pack2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    }

}
