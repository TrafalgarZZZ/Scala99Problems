package binarytree {
    sealed abstract class Tree[+T] {
        def isMirrorOf[V](tree: Tree[V]): Boolean
        def isSymmetric: Boolean
    }

    case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
        override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

        override def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
            case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
            case _ => false
        }

        override def isSymmetric: Boolean = left.isMirrorOf(right)
    }

    case object End extends Tree[Nothing] {
        override def toString: String = "."

        override def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End

        override def isSymmetric: Boolean = true
    }

    object Node {
        def apply[T](value: T): Node[T] = new Node(value, End, End)
        def apply[T](value: T, left: Tree[T], right: Tree[T]) = new Node(value, left, right)
    }

}
