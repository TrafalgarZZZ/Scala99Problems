class S99Int(val start: Int) {
    def isPrime: Boolean = {
        (start > 1) && (2 to Math.sqrt(start).toInt forall {start % _ != 0})
    }

    def isPrime1: Boolean =
        (start > 1) && (S99Int.primes takeWhile {_ <= Math.sqrt(start)} forall {start % _ != 0})

    def isCoprimeTo(that: Int): Boolean = S99Int.gcd(start, that) == 1

    def totient = (1 to start).count(this.isCoprimeTo)

    def totientFast = primeFactorMultiplicity.foldLeft(1)((a, b) => {
        a * (b._1 - 1) * fastPower(b._1, b._2 - 1)
    })


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

    def goldbach = {
        S99Int.primes.takeWhile(_ < start) find {p => new S99Int(start - p).isPrime1} match {
            case None => throw new IllegalArgumentException
            case Some(a) => (a, start - a)
        }
    }

    private def fastPower(a: Int, b: Int): Int = {
        if(b == 0) return 1
        if(b == 1) return a
        val temp = fastPower(a, b / 2)
        if((b & 1) == 1) {
            temp * temp * a
        } else {
            temp * temp
        }
    }
}

object S99Int {
    // explicitly import implicit conversion feature using the import clause below
    import scala.language.implicitConversions
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2) filter {_.isPrime1})

    @scala.annotation.tailrec
    def gcd(a: Int, b: Int): Int = if(a % b == 0) b else gcd(b, a % b)

    def listPrimesinRange(range: Range) = primes.dropWhile(_ < range.start).takeWhile(_ <= range.end).toList

    def main(args: Array[String]): Unit = {
        //p31
        Console println 2.isPrime
        Console println 4.isPrime1

        //p32
        Console println gcd(36, 64)

        //p33
        Console println 35.isCoprimeTo(64)

        //p34
        Console println 315.totient

        //p35
        Console println 315.primeFactors

        //p36
        Console println 315.primeFactorMultiplicity

        //p37
        Console println 315.totientFast

        //p38
        var curTime = System.currentTimeMillis()
        Console println 10090000.totient
        println(s"totient takes ${System.currentTimeMillis() - curTime} ms")

        curTime = System.currentTimeMillis()
        Console println 10090000.totientFast
        println(s"totientFast takes ${System.currentTimeMillis() - curTime} ms")

        //p39
        Console println listPrimesinRange(7 to 31)

        //p40
        Console println 16.goldbach
    }

}
