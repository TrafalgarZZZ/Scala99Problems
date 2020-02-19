object Logic {
  def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
      case (true, true) => true
      case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def not(a: Boolean): Boolean = a match {
    case false => true
    case _ => false
  }

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

  def equ(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case (false, false) => true
    case _ => false
  }

  def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))

  def impl(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, false) => false
    case _ => true
  }

  def table2(func: (Boolean, Boolean) => Boolean): Unit = {
    println(
      s"""
        | A     B     Res
        | True  True  ${func(true, true)}
        | True  False ${func(true, false)}
        | False True  ${func(false, true)}
        | False False ${func(false, false)}
      """.stripMargin)
  }

  def main(args: Array[String]): Unit = {
    table2((a, b) => a and (a or not(b)))
  }


  implicit class BooleanLike(b: Boolean) {
    def and(other: Boolean): Boolean = Logic.and(b, other)

    def or(other: Boolean): Boolean = Logic.or(b, other)

    //...
  }


}
