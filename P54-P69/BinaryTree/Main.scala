package binarytree

object Main {
    def main(args: Array[String]): Unit = {
        //P56
        Console println Node('a', Node('b'), Node('c')).isSymmetric
        Console println Node('a', Node('b', End, Node('c')), Node('d')).isSymmetric

        //p57
        val testLs = List(3, 5, 7, 1, 2, 3, 6, -3, 10)
        Console println BinarySearchTree.fromList(testLs)

        val testLs2 = List("aaa", "abc", "ddd", "acc", "xcc", "123")
        val x = BinarySearchTree.fromList(testLs2)
        println(s"x: ${x.getClass} = ${x.toString}")

        //p61
        Console println Node('x', Node('x'), End).leafCount
        Console println Node('x', Node('x', Node('x'), Node('x')), Node('x')).leafCount

        //p61A
        Console println Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList

    }
}
