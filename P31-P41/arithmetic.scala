class S99Int(val start: Int) {
    def isPrime: Boolean = {
        (start > 1) && (2 to Math.sqrt(start).toInt forall {start % _ != 0})
    }

    def isPrime1: Boolean =
        (start > 1) && (S99Int.primes takeWhile {_ <= Math.sqrt(start)} forall {start % _ != 0})

    def isCoprimeTo(that: Int): Boolean = S99Int.gcd(start, that) == 1

    def totient = (1 to start).count(this.isCoprimeTo)

    def primeFactors: List[Int] = {
        start match {
            case 1 => Nil
            case _ => {
                val elem = S99Int.primes.find{start % _ == 0}.get
                elem :: new S99Int(start / elem).primeFactors
            }
        }
    }

    def primeFactorMultiplicity: List[(Int, Int)] = {
        def primeFactorMultiplicit(ls: List[Int]): List[(Int, Int)] = {
            if(ls.isEmpty)
                Nil
            else {
                val (first, last) = ls.span(_ == ls.head)
                (first.head, first.length) :: primeFactorMultiplicit(last)
            }
        }
        primeFactorMultiplicit(primeFactors)
    }
}

object S99Int {
    // explicitly import implicit conversion feature using the import clause below
    import scala.language.implicitConversions
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2) filter {_.isPrime1})

    @scala.annotation.tailrec
    def gcd(a: Int, b: Int): Int = if(a % b == 0) b else gcd(b, a % b)

    def main(args: Array[String]): Unit = {
        //p31
        Console println 2.isPrime
        Console println 4.isPrime1

        //p32
        Console println gcd(36, 64)

        //p33
        Console println 35.isCoprimeTo(64)

        //p34
        Console println 10.totient

        //p35
        Console println 315.primeFactors

        //p36
        Console println 315.primeFactorMultiplicity

    }

}
