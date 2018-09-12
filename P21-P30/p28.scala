object P28 {
    def lsort[A](ls: List[List[A]]): List[List[A]] = {
        ls sortWith {_.length < _.length}
    }

    def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
       import scala.collection.mutable.{Map => MutableMap}
       val mp = MutableMap.empty[Int, Int]
       ls foreach { lss =>
           val len = lss.length
           mp.get(len) match {
               case Some(n) => mp(len) = n + 1
               case None => mp += (lss.length -> 1)
           }
       }
       ls.sortWith((a, b) => mp(a.length) < mp(b.length))
   }

   //solution provided by website author
   //but is not compatible with Scala version 2.12.6
   // import P10.encode
   // def lsortFreq1[A](ls: List[List[A]]): List[List[A]] = {
   //     var freqs = Map(encode(ls map { _.length } sort { _ < _ }) map {_.swap}:_*)
   //     ls sort {(e1, e2) => freqs(e1.length) < freqs(e2.length)}
   // }

    def main(args: Array[String]): Unit = {
        Console println lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
        Console println lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    }

}
