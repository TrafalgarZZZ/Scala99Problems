object P01 {
    def last[A](ls: List[A]) = ls.last
    def lastRecursive[A](ls: List[A]): A = ls match {
        case h :: Nil => h
        case _ :: tail => lastRecursive(tail)
        case _ => throw new NoSuchElementException
    }

    def main(args: Array[String]): Unit = {
        val ls = List("233", "344", "566")
        Console println last(ls)
        Console println last(ls reverse)
        Console println lastRecursive(ls)
    }
}
