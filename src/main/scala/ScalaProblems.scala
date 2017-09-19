object ScalaProblems extends App {
  // HELPERS
  def title(s: String) = println(s"\n$s\n")

  def last(l: List[Int]): Int = l match {
    case x :: Nil => x
    case _ => last(l.tail)
  }

  val lastData = List(1, 1, 2, 3, 5, 8)
  title("LAST")
  println("Native -               " + (last(lastData) == 8))
  println("Scala out of the box - " + (lastData.last == 8))

  def penultimate(l: List[Any]): Any = l match {
    case x :: tail if tail.length == 1 => x
    case _ => penultimate(l.tail)
  }

  val penultimateData = List(1, 1, 2, 3, 5, 8)
  title("PENULTIMATE")
  println("Native -               " + (penultimate(penultimateData) == 5))
  println("Scala out of the box - " + (penultimateData.reverse(1) == 5))

  def nth(n: Int, l: List[Any]): Any = n match {
    case 0 => l.head
    case _ => nth(n - 1, l.tail)
  }

  val nthData = List(1, 1, 2, 3, 5, 8)
  title("NTH")
  println("Native -               " + (nth(2, nthData) == 2))
  println("Scala out of the box - " + (nthData(2) == 2))

  def length(l: List[Any]): Int = l match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }

  val lengthData = List(1, 1, 2, 3, 5, 8)
  title("LENGTH")
  println("Native -               " + (length(lengthData) == 6))
  println("Scala out of the box - " + (lengthData.length == 6))

  def reverse(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case x :: tail => reverse(tail) :+ x
  }

  val reverseData = List(1, 1, 2, 3, 5, 8)
  title("REVERSE")
  println("Native -               " + (reverse(reverseData) == List(8, 5, 3, 2, 1, 1)))
  println("Scala out of the box - " + (reverseData.reverse == List(8, 5, 3, 2, 1, 1)))

  def isPalindrome(l: List[Any]): Boolean = {
    def aa(ll: List[Any]): Boolean = ll match {
      case head :: Nil => true
      case head :: tail if head != tail.last => false
      case _ => aa(ll.tail.dropRight(1))
    }
    if(l.length % 2 == 1) false
    aa(l)
  }

  def isPalindrome2(l: List[Any]): Boolean = {
    val firstHalf = l.take((l.length - 1) / 2)
    val secondHalf = l.takeRight((l.length - 1) / 2)
    firstHalf == secondHalf.reverse
  }

  val palindromeData = List(1, 2, 3, 2, 1)
  title("PALINDROME")
  println("Native -               " + (isPalindrome(palindromeData) == true))
  println("Scala out of the box - " + (isPalindrome2(palindromeData) == true))

  def flatten(l: List[Any]): List[Any] = l match {
    case(x:List[Any])::tail => flatten(x) ::: flatten(tail)
    case (x:Any)::tail => x :: flatten(tail)
    case _ => Nil
  }

  val flattenData = List(List(1, 1), 2, List(3, List(5, 8)))
  title("FLATTEN")
  println("Native -               " + (flatten(flattenData) == List(1, 1, 2, 3, 5, 8)))
  println("Scala out of the box - same as native implementation")

  def compress(prev: Any = None, l: List[Any]): List[Any] = l match {
    case (x:Any)::tail if prev == x => compress(x, tail)
    case (x:Any)::tail => x :: compress(x, tail)
    case _ => Nil
  }

  def compress2(l: List[Char]) = l.foldLeft(List[Char]())((a, b) => if (a != Nil && a.last == b) a else a :+ b)

  val compressData = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  title("COMPRESS")
  println("Native -               " + (compress(l = compressData) == List('a', 'b', 'c', 'a', 'd', 'e')))
  println("Scala out of the box - " + (compress2(compressData) == List('a', 'b', 'c', 'a', 'd', 'e')))

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

  val packData = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val packReturnData = List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e'))
  title("PACK")
  println("Native -               " + (pack(packData) == packReturnData))
  println("Scala out of the box - " + (pack2(packData) == packReturnData))

  def encode(l: List[Any]): List[(Int, Any)] = {
    def recourse(ls: List[List[Any]]): List[(Int, Any)] = ls match {
      case x :: tail => (x.length, x.head) :: recourse(tail)
      case _ => Nil
    }
    recourse(pack(l))
  }

  def encode2(l: List[Any]): List[(Int, Any)] = pack2(l).map(ls => (ls.length, ls.head))

  val encodeData = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val encodeReturnData = List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))
  title("ENCODE")
  println("Native -               " + (encode(encodeData) == encodeReturnData))
  println("Scala out of the box - " + (encode2(encodeData) == encodeReturnData))

  def encodeModified(l: List[Any]): List[Any] = {
    def recourse(ls: List[List[Any]]): List[Any] = ls match {
      case x :: tail if x.length == 1 => x.head :: recourse(tail)
      case x :: tail => (x.length, x.head) :: recourse(tail)
      case _ => Nil
    }
    recourse(pack(l))
  }

  def encodeModified2(l: List[Char]): List[Any] = {
    pack2(l).map { entry =>
      if(entry.length == 1) {
        entry.head
      } else {
        (entry.length, entry.head)
      }
    }.toList
  }

  val encodeModifiedData = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val encodeModifiedReturnData = List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e'))
  title("ENCODEMODIFIED")
  println("Native -               " + (encodeModified(encodeModifiedData) == encodeModifiedReturnData))
  println("Scala out of the box - " + (encodeModified2(encodeModifiedData) == encodeModifiedReturnData))
}