object P03 {
    def kth[A](ls: List[A], k: Int): A = {
        if(k < 0) throw new NoSuchElementException
        if(k == 0) ls.head
        else kth(ls.tail, k - 1)
    }

    def kthRecursive[A](ls: List[A], k: Int): A = (ls, k) match {
        case (_, b) if b < 0 => throw new NoSuchElementException
        case (Nil, _) => throw new NoSuchElementException
        case (h :: _, 0) => h
        case (_ :: tail, _) => kthRecursive(tail, k - 1)
    }


    def main(args: Array[String]): Unit = {
        val ls = List(3, 2, 1, 5, 7, 9)
        Console println ls(3)
        Console println kthRecursive(Nil, 3)
    }

}
