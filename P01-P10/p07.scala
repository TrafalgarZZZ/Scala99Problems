import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer

object P07 {
  def flattenFuncOps(ls: List[Any]): List[Any] = {
    ls.foldLeft(ListBuffer[Any]()) {
      case (xs, ys: List[Any]) => xs.appendAll(flattenFuncOps(ys)); xs
      case (xs, y) => xs += y
    }.toList
  }

  def flattenNaive(ls: List[Any]): List[Any] = {
    ls match {
      case (h :: xs) :: tail => flattenNaive(h :: xs) ::: flattenNaive(tail)
      case h :: tail => h :: flattenNaive(tail)
      case Nil => Nil
    }
  }

  def main(args: Array[String]): Unit = {
//    val ls = List(List(1, 1), 2, List(3, List(5, List(8, 13))))
    val ls = List()
    println(flattenFuncOps(ls))
    println(flattenNaive(ls))
//    println(flattenFuncOps(ls))
  }
}