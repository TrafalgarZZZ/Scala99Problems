object P49 {
  def gray(n: Int): List[String] = n match {
    case 1 => List("0", "1")
    case _ => {
      val last = gray(n - 1)
      last.map("0" + _) ++ last.map("1" + _)
    }
  }

  def main(args: Array[String]): Unit = {
    println(gray(6))
  }
}
