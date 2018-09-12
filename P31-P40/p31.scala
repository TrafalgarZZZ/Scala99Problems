class S99Int(val start: Int) {
    def isPrime: Boolean = {
        (start > 1) && (2 to Math.sqrt(start).toInt forall {start % _ != 0})
    }

    def isPrime1: Boolean =
        (start > 1) && (S99Int.primes takeWhile {_ <= Math.sqrt(start)} forall {start % _ != 0})
}

object S99Int {
    // explicitly import implicit conversion feature using the import clause below
    val primes = Stream.cons(2, Stream.from(3, 2) filter {_.isPrime1})

    import scala.language.implicitConversions
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def main(args: Array[String]): Unit = {
        Console println 2.isPrime
        Console println 4.isPrime1
    }
}
