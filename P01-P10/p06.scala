object P06 {
    def isPalindrome[A](ls: List[A]): Boolean = ls.reverse == ls
    def isPalindromeRecursive[A](ls: List[A]): Boolean = {
        if(ls.isEmpty || ls.length == 1) true
        else if(ls.head != ls.last) false
        else isPalindromeRecursive(ls.tail.init)
    }
    def main(args: Array[String]): Unit = {
        println(isPalindromeRecursive(List(1, 2, 3, 4, 2, 1)))
    }
}
