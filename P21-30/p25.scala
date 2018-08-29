import util.Random
import scala.reflect.ClassTag
object P25 {
    //same in P23
    def randomSelect[A](n: Int, ls: List[A]): List[A] = {
        def randomSelectR(n:Int, ls: List[A], r: Random): List[A] =
            if(n <= 0) Nil
            else {
                val (rest, e) = removeAt1(r.nextInt(ls.length), ls)
                e :: randomSelectR(n - 1, rest, r)
            }
        randomSelectR(n, ls, new Random )
    }
    def removeAt1[A](n: Int, ls: List[A]) = {
        val (pre, post) = ls.splitAt(n)
        (pre ::: post.tail, post.head)
    }
    // end for P23

    //
    def randomPermute[A](ls: List[A]) = {
        randomSelect(ls.length, ls)
    }

    // a faster solution
    //todo: What is this ClassTag work for?And why ClassManifest is deprecated?
    def randomPermute1[A: ClassTag](ls: List[A]): List[A] = {
        val rand = new Random
        val a = ls.toArray
        for(i <- a.length - 1 to 1 by -1) {
            val i1 = rand.nextInt(i + 1)
            val t = a(i)
            a.update(i, a(i))
            a.update(i1, t)
        }
        a.toList
    }

    def main(args: Array[String]): Unit = {
        Console println randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
        // Console println randomPermute1[Symbol](List('a, 'b, 'c, 'd, 'e, 'f))

    }

}
