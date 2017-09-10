object ScalaProblems extends App {
  def last(l: List[Int]): Int = l match {
    case x :: Nil => x
    case _ => last(l.tail)
  }

  println("LAST")
  println("Native -               " + (last(List(1, 1, 2, 3, 5, 8)) == 8))
  println("Scala out of the box - " + (List(1, 1, 2, 3, 5, 8).last == 8))

  def penultimate(l: List[Any]): Any = l match {
    case x :: tail if tail.length == 1 => x
    case _ => penultimate(l.tail)
  }

  println("PENULTIMATE")
  println("Native -               " + (penultimate(List(1, 1, 2, 3, 5, 8)) == 5))
  println("Scala out of the box - " + (List(1, 1, 2, 3, 5, 8).reverse(1) == 5))

  def nth(n: Int, l: List[Any]): Any = n match {
    case 0 => l.head
    case _ => nth(n - 1, l.tail)
  }

  println("NTH")
  println("Native -               " + (nth(2, List(1, 1, 2, 3, 5, 8)) == 2))
  println("Scala out of the box - " + (List(1, 1, 2, 3, 5, 8)(2) == 2))

  def length(l: List[Any]): Int = l match {
    case Nil => 0
    case x :: tail => 1 + length(tail)
  }

  println("LENGTH")
  println("Native -               " + (length(List(1, 1, 2, 3, 5, 8)) == 6))
  println("Scala out of the box - " + (List(1, 1, 2, 3, 5, 8).length == 6))

  def reverse(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case x :: tail => reverse(tail) :+ x
  }

  println("Native -               " + (reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1)))
  println("Scala out of the box - " + (List(1, 1, 2, 3, 5, 8).reverse == List(8, 5, 3, 2, 1, 1)))

  def isPalindrome(l: List[Any]): Boolean = {
    def aa(ll: List[Any]): Boolean = ll match {
      case head :: Nil => true
      case head :: tail if head != tail.last => false
      case _ => aa(ll.tail.dropRight(1))
    }
    if(l.length % 2 == 1) false
    aa(l)
  }

  println("PALINDROME")
  println("Native -               " + (isPalindrome(List(1, 2, 3, 2, 1)) == true))
  val palindromeList = List(1, 2, 3, 2, 1)
  val firstHalf = palindromeList.take((palindromeList.length - 1) / 2)
  val secondHalf = palindromeList.takeRight((palindromeList.length - 1) / 2)
  println("Scala out of the box - " + (firstHalf == secondHalf.reverse))

  def flatten(l: List[Any]): List[Any] = l match {
    case(x:List[Any])::tail => flatten(x) ::: flatten(tail)
    case (x:Any)::tail => x :: flatten(tail)
    case _ => Nil
  }

  println("FLATTEN")
  println("Native -               " + (flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8)))
  println("Scala out of the box - same as native")

  def compress(prev: Any = None, l: List[Any]): List[Any] = l match {
    case (x:Any)::tail if prev == x => compress(x, tail)
    case (x:Any)::tail => x :: compress(x, tail)
    case _ => Nil
  }

  println("COMPRESS")
  println("Native -               " + (compress(l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) == List('a', 'b', 'c', 'a', 'd', 'e')))
  println("Scala out of the box - " + (List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e').foldLeft(List[Char]())((a, b) => if (a != Nil && a.last == b) a else a :+ b) == List('a', 'b', 'c', 'a', 'd', 'e')))

  def packHelper(prevItems: List[Any], ll: List[Any]): (List[Any], List[Any]) = ll match {
    case x :: tail if prevItems == Nil || x == prevItems.last => packHelper(x :: prevItems, tail)
    case _ => (prevItems, ll)
  }

  def pack(l: List[Any]): List[List[Any]] = l match {
    case Nil => Nil
    case _ => {
      val (list, next) = packHelper(Nil, l)
      list :: pack(next)
    }
  }

  def pack2(l: List[Any]): List[List[Any]] = l match {
    case Nil => Nil
    case x :: _ => {
      val (list, ttail) = l.span(_ == x)
      list :: pack2(ttail)
    }
  }

  println("PACK")
  println("Native -               " + (pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) == List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e'))))
  println("Scala out of the box - " + (pack2(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) == List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e'))))

  def encode(l: List[Any]): List[(Int, Any)] = {
    def recourse(ls: List[List[Any]]): List[(Int, Any)] = ls match {
      case x :: tail => (x.length, x.head) :: recourse(tail)
      case _ => Nil
    }
    recourse(pack(l))
  }

  def encode2(l: List[Any]): List[(Int, Any)] = pack2(l).map(ls => (ls.length, ls.head))

  println("ENCODE")
  println("Native -               " + (encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) == List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))))
  println("Scala out of the box - " + (encode2(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) == List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))))
}