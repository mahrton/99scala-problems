import Lists.removeAt

import scala.collection.SortedSet

object Lists extends App {
  // HELPERS
  def title(s: String) = println(s"\n$s\n")
  def native(b: Boolean) = println("Native -               " + b)
  def scala(b: Boolean) = println("Scala out of the box - " + b)

  def last(l: List[Int]): Int = l match {
    case x :: Nil => x
    case _ => last(l.tail)
  }

  val lastData = List(1, 1, 2, 3, 5, 8)
  title("LAST")
  native(last(lastData) == 8)
  scala(lastData.last == 8)

  def penultimate(l: List[Any]): Any = l match {
    case x :: tail if tail.length == 1 => x
    case _ => penultimate(l.tail)
  }

  val penultimateData = List(1, 1, 2, 3, 5, 8)
  title("PENULTIMATE")
  native(penultimate(penultimateData) == 5)
  scala(penultimateData.reverse(1) == 5)

  def nth(n: Int, l: List[Any]): Any = n match {
    case 0 => l.head
    case _ => nth(n - 1, l.tail)
  }

  val nthData = List(1, 1, 2, 3, 5, 8)
  title("NTH")
  native(nth(2, nthData) == 2)
  scala(nthData(2) == 2)

  def length(l: List[Any]): Int = l match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }

  val lengthData = List(1, 1, 2, 3, 5, 8)
  title("LENGTH")
  native(length(lengthData) == 6)
  scala(lengthData.length == 6)

  def reverse(l: List[Any]): List[Any] = l match {
    case Nil => Nil
    case x :: tail => reverse(tail) :+ x
  }

  val reverseData = List(1, 1, 2, 3, 5, 8)
  title("REVERSE")
  native(reverse(reverseData) == List(8, 5, 3, 2, 1, 1))
  scala(reverseData.reverse == List(8, 5, 3, 2, 1, 1))

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
  native(isPalindrome(palindromeData))
  scala(isPalindrome2(palindromeData))

  def flatten(l: List[Any]): List[Any] = l match {
    case(x:List[Any])::tail => flatten(x) ::: flatten(tail)
    case (x:Any)::tail => x :: flatten(tail)
    case _ => Nil
  }

  val flattenData = List(List(1, 1), 2, List(3, List(5, 8)))
  title("FLATTEN")
  native(flatten(flattenData) == List(1, 1, 2, 3, 5, 8))
  println("Scala out of the box - same as native implementation")

  def compress(prev: Any = None, l: List[Any]): List[Any] = l match {
    case (x:Any)::tail if prev == x => compress(x, tail)
    case (x:Any)::tail => x :: compress(x, tail)
    case _ => Nil
  }

  def compress2(l: List[Char]) = l.foldLeft(List[Char]())((a, b) => if (a != Nil && a.last == b) a else a :+ b)

  val compressData = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  title("COMPRESS")
  native(compress(l = compressData) == List('a', 'b', 'c', 'a', 'd', 'e'))
  scala(compress2(compressData) == List('a', 'b', 'c', 'a', 'd', 'e'))

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
  native(pack(packData) == packReturnData)
  scala(pack2(packData) == packReturnData)

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
  native(encode(encodeData) == encodeReturnData)
  scala(encode2(encodeData) == encodeReturnData)

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
  native(encodeModified(encodeModifiedData) == encodeModifiedReturnData)
  scala(encodeModified2(encodeModifiedData) == encodeModifiedReturnData)

  def decodeHelper(a: Int, b: Char): List[Char] = a match {
    case 0 => Nil
    case _ => b :: decodeHelper(a - 1, b)
  }

  def decode(l : List[(Int, Char)]): List[Char] = l match {
    case head :: tail => decodeHelper(head._1, head._2) ::: decode(tail)
    case _ => Nil
  }

  def decode2(l : List[(Int, Char)]): List[Char] = l.flatMap(t => List().padTo(t._1, t._2))

  val decodeData = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
  val decodeReturnData = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  title("DECODE")
  native(decode(decodeData) == decodeReturnData)
  scala(decode2(decodeData) == decodeReturnData)

  def encodeDirect(l: List[Char], last: List[Char] = Nil): List[(Int, Char)] = l match {
    case Nil => (last.length, last.head) :: Nil
    case x :: tail if last == Nil || x == last.head => encodeDirect(tail, x :: last)
    case x :: tail => (last.length, last.head) :: encodeDirect(tail, List(x))
  }

  def encodeDirect2(l: List[Char]): List[(Int, Char)] = {
    if(l.isEmpty) {
      Nil
    } else {
      val ll = l.takeWhile(c => c == l.head)
      (ll.length, ll.head) :: encodeDirect2(l.drop(ll.length))
    }
  }

  val encodeDirectData = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val encodeDirectReturnData = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
  title("ENCODEDIRECT")
  native(encodeDirect(encodeDirectData) == encodeDirectReturnData)
  scala(encodeDirect2(encodeDirectData) == encodeDirectReturnData)

  def duplicate(l: List[Char]): List[Char] = l match {
    case x :: tail => x :: x :: duplicate(tail)
    case Nil => Nil
  }

  def duplicate2(l: List[Char]): List[Char] = l.flatMap(x => List(x, x))

  val duplicateData = List('a', 'b', 'c', 'c', 'd')
  val duplicateReturnData = List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd')
  title("DUPLICATE")
  native(duplicate(duplicateData) == duplicateReturnData)
  scala(duplicate2(duplicateData) == duplicateReturnData)

  def duplicateN(num: Int, l: List[Char]): List[Char] = l match {
    case x :: tail => (for( i <- 0 until num ) yield x).toList ::: duplicateN(num, tail)
    case Nil => Nil
  }

  def duplicateN2(num: Int, l: List[Char]): List[Char] = l.flatMap(x => List().padTo(num, x))

  val duplicateNData = List('a', 'b', 'c', 'c', 'd')
  val duplicateNReturnData = List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd')
  title("DUPLICATEN")
  native(duplicateN(3, duplicateNData) == duplicateNReturnData)
  scala(duplicateN2(3, duplicateNData) == duplicateNReturnData)

  def drop(num: Int, l: List[Char], counter: Int = 0): List[Char] = l match {
    case x :: tail if counter + 1 == num => drop(num, tail, 0)
    case x :: tail => x :: drop(num, tail, counter + 1)
    case Nil => Nil
  }

  def drop2(num: Int, l: List[Char]): List[Char] = l.zipWithIndex.filterNot(t => (t._2 + 1) % num == 0).map(_._1)

  val dropData = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
  val dropReturnData = List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k')
  title("DROP")
  native(drop(3, dropData) == dropReturnData)
  scala(drop2(3, dropData) == dropReturnData)

  def split(num: Int, l: List[Char], temp: List[Char] = Nil): (List[Char], List[Char]) = l match {
    case _ if num == 0 => (temp, l)
    case x :: tail => split(num - 1, tail, temp :+ x)
     case Nil => (temp, l)
  }

  def split2(num: Int, l: List[Char]): (List[Char], List[Char]) = l.splitAt(num)

  val splitData = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
  val splitReturnData = (List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
  title("SPLIT")
  native(split(3, splitData) == splitReturnData)
  scala(split2(3, splitData) == splitReturnData)

  def slice(begin: Int, end: Int, l: List[Char], counter: Int = 0): List[Char] = l match {
    case x :: tail if counter >= begin && counter < end => x :: slice(begin, end, tail, counter + 1)
    case _ if counter + 1 > end => Nil
    case _ :: tail => slice(begin, end, tail, counter + 1)
    case Nil => Nil
  }

  def slice2(begin: Int, end: Int, l: List[Char]): List[Char] = l.slice(begin, end)

  val sliceData = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
  val sliceReturnData = List('d', 'e', 'f', 'g')
  title("SLICE")
  native(slice(3, 7, sliceData) == sliceReturnData)
  scala(slice2(3, 7, sliceData) == sliceReturnData)


  def rotate(shift: Int, l: List[Char]): List[Char] = shift match {
    case x if x > 0 => rotate(shift - 1, l.tail :+ l.head)
    case x if x < 0 => rotate(shift + 1, l.last :: l.init)
    case 0 => l
  }

  def rotate2(shift: Int, l: List[Char]): List[Char] = shift match {
    case x if x < 0 => l.takeRight(Math.abs(x)) ::: l.dropRight(Math.abs(x))
    case x if x > 0 => l.drop(x) ::: l.take(x)
    case 0 => l
  }

  val rotateData = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
  val rotateReturnData1 = List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')
  val rotateReturnData2 = List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
  title("ROTATE")
  native(rotate(3, rotateData) == rotateReturnData1 && rotate(-2, rotateData) == rotateReturnData2)
  scala(rotate2(3, rotateData) == rotateReturnData1 && rotate2(-2, rotateData) == rotateReturnData2)


  def removeAt(at: Int, l: List[Any]): Option[(List[Any], Any)] = {
    def rec(at: Int, list: List[Any]): List[Any] = list match {
      case Nil => Nil
      case _ :: tail if at == 0 => rec(at - 1, tail)
      case x :: tail => x :: rec(at - 1, tail)
    }
    if(at >= 0 && at <= l.length) Some((rec(at, l), l(at)))
    else None
  }

  def removeAt2(at: Int, l: List[Char]): Option[(List[Char], Char)] = {
    if(at >= 0 && at <= l.length) Some((l.take(at) ++ l.drop(at + 1), l(at)))
    else None
  }

  val removeAtData = List('a', 'b', 'c', 'd')
  val removeAtReturnData = (List('a', 'c', 'd'), 'b')
  title("REMOVE AT")
  native(removeAt(3, removeAtData) == Some((List('a', 'b', 'c'),'d')))
  scala(removeAt2(3, removeAtData) == Some((List('a', 'b', 'c'),'d')))


  def insertAt(c: Char, at: Int, list: List[Char]): List[Char] = {
    def rec(aat: Int, ll: List[Char]): List[Char] = ll match {
      case Nil => Nil
      case x :: tail if aat == 0 => c :: x :: rec(aat - 1, tail)
      case x :: tail => x :: rec(aat - 1, tail)
    }
    if(at >= 0 && at < list.length) {
      rec(at, list)
    } else list
  }

  def insertAt2(c: Char, at: Int, l: List[Char]): List[Char] = {
    if(at >= 0 && at < l.length) {
      val (a, b) = l.splitAt(at)
      (a :+ c) ::: b
    } else l
  }

  val insertAtData = List('a', 'b', 'c', 'd')
  val insertAtReturnData = (List('a', 'c', 'd'), 'b')
  title("INSERT AT")
  native(insertAt('n', 1, insertAtData) == List('a', 'n', 'b', 'c', 'd'))
  scala(insertAt2('n', 1, removeAtData) == List('a', 'n', 'b', 'c', 'd'))


  def range(x: Int, y: Int): List[Int] = (x, y) match {
    case (xx, yy) if xx == yy => xx :: Nil
    case (xx, yy) => xx :: range(xx + 1, yy)
  }

  def range2(x: Int, y: Int): List[Int] = (x to y).toList

  val rangeReturnData = List(4, 5, 6, 7, 8, 9)
  title("RANGE")
  native(range(4, 9) == rangeReturnData)
  scala(range2(4, 9) == rangeReturnData)


  def randomSelect(i: Int, l: List[Any]): List[Any] = {
    if(i == 0 || l == Nil) Nil
    else {
      val dd = Math.round(Math.round(Math.random() * (l.length - 1)))
      removeAt(dd, l) match {
        case Some(x) => x._2 :: randomSelect(i - 1, x._1)
        case None => randomSelect(i, l)
      }
    }
  }

  def randomSelect2(i: Int, l: List[Any]): List[Any] = {
    var s = SortedSet[Int]()
    while(s.size < i) {
      s += Math.round(Math.round(Math.random() * (l.length - 1)))
    }
    s.map(l(_)).toList
  }

  val randomSelectData = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
  title("RANDOMSELECT")
  val randomSelectReturn1 = randomSelect(4, randomSelectData)
  native(randomSelectReturn1.forall(aa => randomSelectData.contains(aa)))
  val randomSelectReturn2 = randomSelect2(4, randomSelectData)
  scala(randomSelectReturn1.forall(aa => randomSelectData.contains(aa)))


  def lotto(a: Int, b: Int): List[Any] = randomSelect(a, range(1, b))

  def lotto2(a: Int, b: Int): List[Any] = randomSelect2(a, range2(1, b))

  title("LOTTO")
  val lottoResults1 = lotto(6, 49)
  native(lottoResults1.length == 6 && lottoResults1.forall(aa => aa.asInstanceOf[Int] < 50))
  val lottoResults2 = lotto(6, 49)
  scala(lottoResults2.length == 6 && lottoResults2.forall(aa => aa.asInstanceOf[Int] < 50))


  def randomPermute(l: List[Any]): List[Any] = randomSelect(l.length, l)

  def randomPermute2(l: List[Any]): List[Any] = randomSelect2(l.length, l)

  val randomPermuteData = List('a', 'b', 'c', 'd', 'e', 'f')
  title("RANDOMPERMUTE")
  native(randomPermute(randomPermuteData).asInstanceOf[List[Char]].sorted == randomPermuteData)
  scala(randomPermute2(randomPermuteData).asInstanceOf[List[Char]].sorted == randomPermuteData)
}