object P50 {
  def huffman(ls: List[(String, Int)]): List[(String, String)] = {
    @scala.annotation.tailrec
    type TNode = (String, Int, List[(String, String)])
    def huffmanHelper(xs: List[TNode]): TNode = xs match {
      case h :: Nil => h
      case _ =>
        val (mergeNodes, tail) = xs.sortBy(_._2).splitAt(2)
        val next = mergeNodes.reduce[TNode]{
          case ((xstr, xw, xtree), (ystr, yw, ytree)) =>
            (xstr + ystr,
              xw + yw,
              xtree.map {case (name, repr) => (name, "0" + repr)} ::: ytree.map {case (name, repr) => (name, "1" + repr)})
        } :: tail
        huffmanHelper(next)
    }

    ls match {
      case Nil => List()
      case h :: Nil => List((h._1, "0"))
      case _ => huffmanHelper(ls.map {
        case (name, w) => (name, w, List((name, "")))
      })._3
    }
  }

  def main(args: Array[String]): Unit = {
    println(huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 6), ("f", 5))))
  }

}
