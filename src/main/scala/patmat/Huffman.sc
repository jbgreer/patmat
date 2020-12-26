import patmat.Huffman._

object Foo {

  val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)

  val et1 = patmat.Huffman.encode(t1)("ab".toList)
  /*
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def chars(ct: CodeTree) : List[Char] = ct match {
    case Fork(_, _, chars, _) => chars
    case Leaf(char, _) => List(char)
  }

  def weight(ct: CodeTree) : Int = ct match {
    case Fork(_, _, _, wt) => wt
    case Leaf(_, wt) => wt
  }

  def times(chars: List[Char]) : List[(Char, Int)] =
    chars.map(c => (c, chars.count(_ == c))).distinct.sortBy(l => (l._1, l._2))

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.sortBy(l => (l._2, l._1)).map(l => Leaf(l._1, l._2))
  }

  def combine(trees: List[CodeTree]): List[CodeTree] =
    trees match {
      //case left :: right :: tail => Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right)) :: tail
      case left :: right :: tail =>
        (Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right)) :: tail).sortWith(weight(_) < weight(_))
      case _ => trees
    }

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def until[T](singleton: T => Boolean, combine: T => T)(list: T): T =
    if (singleton(list)) list
    else until(singleton, combine)(combine(list))

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int


 /* def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    def decodeacc(t: CodeTree, b: List[Bit], acc: List[Char]) : List[Char] =
      tree match {
        case Leaf(ch, _) =>
          if (b == Nil) ch :: acc
          else decodeacc(t, b, ch :: acc)
        case Fork(l, r, _, _) =>
          if (b.head == 0) decodeacc(l, b.tail, acc)
          else decodeacc(r, b.tail, acc)
      }

    decodeacc(tree, bits, Nil).reverse
  }
*/
 def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
   def f(t: CodeTree, bs: List[Bit]): (Char, List[Bit]) =
     t match {
       case Fork(l, r, _, _) =>
         bs match {
           case 0 :: xs => f(l, xs)
           case 1 :: xs => f(r, xs)
           case _ => throw new IllegalArgumentException("not 0 or 1")
         }
       case Leaf(c, _) => (c, bs)
     }
   f(tree, bits) match {
     case (c, Nil) => List(c)
     case (c, bs) => c :: decode(tree, bs)
   }
 }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  decodedSecret
  */

  /*
  val e = List()
  times(e)
  val a = List('a')
  times(a)
  makeOrderedLeafList(times2(a))
  val ab = List('a', 'b')
  times(ab)
  makeOrderedLeafList(times2(ab))
  val aba = List('a', 'b', 'a')
  times2(aba)
  makeOrderedLeafList(times2(aba))
  val baa = List('b', 'a', 'a')
  times2(baa)
  makeOrderedLeafList(times2(baa))
  val baab = List('b', 'a', 'a', 'b')
  times2(baab)
  makeOrderedLeafList(times2(baab))
  val bacab = List('b', 'a', 'c', 'a', 'b')
  makeOrderedLeafList(times2(bacab))
  val abcd = List('a', 'b', 'c', 'd')
  val efgh = List('e', 'f', 'g', 'h')
  abcd :: efgh
  abcd ::: efgh
  until(singleton, combine)(abcd ::: efgh)
  */
}


