package binarytree

object Main {
    def main(args: Array[String]): Unit = {
        //P56
        Console println Node('a', Node('b'), Node('c')).isSymmetric
        Console println Node('a', Node('b', End, Node('c')), Node('d')).isSymmetric
    }
}
