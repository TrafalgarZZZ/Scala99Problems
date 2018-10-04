package binarytree

sealed abstract class BinarySearchTree[+T]{
    def addValue[U >: T](x: U)(implicit ev$1: U => Ordered[U]): BinarySearchTree[U]
}

case class BSTNode[+T](value: T, left: BinarySearchTree[T], right: BinarySearchTree[T]) extends BinarySearchTree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    override def addValue[U >: T](x: U)(implicit ev$1: U => Ordered[U]): BinarySearchTree[U] = {
        if(x < value) BSTNode(value, left.addValue(x), right)
        else BSTNode(value, left, right.addValue(x))
    }
}

case object BSTEnd extends BinarySearchTree[Nothing] {
    override def toString: String = "."

    override def addValue[U >: Nothing](x: U)(implicit ev$1: U => Ordered[U]): BinarySearchTree[U] = BSTNode(x)
}

object BSTNode {
    def apply[T](value: T, left: BinarySearchTree[T], right: BinarySearchTree[T]): BinarySearchTree[T] = new BSTNode(value, left, right)
    def apply[T](value: T): BinarySearchTree[T] = new BSTNode[T](value, BSTEnd, BSTEnd)
}

object BinarySearchTree {
    def fromList[T](ls: List[T])(implicit ev$1: T => Ordered[T]): BinarySearchTree[T] =
        ls.foldLeft(BSTEnd: BinarySearchTree[T])(_.addValue(_))
}

