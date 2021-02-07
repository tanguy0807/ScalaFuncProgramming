/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case f:Fork => f.weight
    case l:Leaf => l.weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case f:Fork => f.chars
    case l:Leaf => l.char :: Nil
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */

  def times(chars: List[Char]): List[(Char, Int)] = {
    //chars.distinct.map(c => (c, chars.count(c_ => c_ == c)))

    // I guess this is computationally more efficient
    def countReduce(chars: List[Char])(pairAccumulator: List[(Char, Int)], char: Char): List[(Char, Int)] = {
      if (pairAccumulator.map(pair => pair._1).contains(char)) {
        //println(char + " already counted")
        pairAccumulator
      } else {
        //println(char + " is new, count:" + chars.count(c => c == char))
        pairAccumulator ::: List((char, chars.count(c => c == char)))
      }
    }
    chars.foldLeft(List():List[(Char, Int)])(countReduce(chars))
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    /*
    // order with character count first (p1._2 <= p2._2), if true then order
    def orderPairs(p1:(Char, Int), p2: (Char, Int)): Boolean = {
      if (p1._2 < p2._2) {
        true
      } else {
        if (p1._2 == p2._2) {
          p1._1 < p2._1
        } else {
          false
        }
      }
    }
    freqs.sortWith((p1, p2) => orderPairs(p1, p2)).map(p => Leaf(p._1, p._2))
    */

    freqs.sortWith((p1, p2) => p1._2 < p2._2).map(p => Leaf(p._1, p._2))

  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = {
    trees.length == 1
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    if (singleton(trees)) {
      trees
      /*
    } else {
      if (weight(trees(0)) < weight(trees(1))) {
        trees.drop(2) ++ List(makeCodeTree(trees(1), trees(0)))
      } else {
        trees.drop(2) ++ List(makeCodeTree(trees(0), trees(1)))
      }
    }
    */
    } else {
      val combinedTrees = List(makeCodeTree(trees(0), trees(1))) ++ trees.drop(2)
      combinedTrees.sortWith((tree1, tree2) => weight(tree1) < weight(tree2))
    }
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (done(trees)) {
      trees
    }
    else {
      until(done, merge)(merge(trees))
    }
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val wordCount = times(chars)
    val trees = makeOrderedLeafList(wordCount)
    until(singleton, combine)(trees)(0)
  }


type Bit = Int

def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

  def traverseFork(fork: Fork, bit: Bit): CodeTree = {
    if (bit == 0) {
      fork.left
    } else {
      fork.right
    }
  }

  def getNextLeaf(tree: CodeTree, bits: List[Bit], intAcc:Int): (Leaf, Int) = {
    tree match {
      case l: Leaf => (l, intAcc)
      case f: Fork => getNextLeaf(traverseFork(f, bits(0)), bits.drop(1), intAcc + 1)
    }
  }

  def decodeAcc(tree:CodeTree, bits:List[Bit], charAcc: List[Char]): List[Char] = {
    if (bits.length > 0) {
      val (leaf:Leaf, numBits:Int) = getNextLeaf(tree, bits, 0)
      decodeAcc(tree, bits.drop(numBits), charAcc ++ chars(leaf))
    } else charAcc
  }

  decodeAcc(tree, bits, Nil)

}


def findLeafBits(tree: CodeTree, char:Char, bitLetterAcc:List[Bit]): List[Bit] = {
  tree match {
    case l: Leaf => if (chars(l).head == char) bitLetterAcc else Nil
    case f: Fork =>
      val resultLeftTree = findLeafBits(f.left, char, bitLetterAcc ++ List(0))
      if (resultLeftTree.nonEmpty) resultLeftTree
      else findLeafBits(f.right, char, bitLetterAcc ++ List(1))
  }
}

def encode(tree: CodeTree)(text: List[Char]): List[Bit] = text match {
  case Nil => Nil
  case x::xs => findLeafBits(tree, x, Nil) ++ encode(tree)(xs)
}


type CodeTable = List[(Char, List[Bit])]

def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
  case Nil => Nil
  case x::xs => if (x._1 == char) x._2 else codeBits(xs)(char)
}

def convertAcc(tree: CodeTree, bitsAcc: List[Bit]): CodeTable = tree match {
  case l:Leaf => List((chars(l).head, bitsAcc))
  case f:Fork => convertAcc(f.left, bitsAcc ++ List(0)) ++ convertAcc(f.right, bitsAcc ++ List(1))
}

def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ++ b

def convert(tree: CodeTree): CodeTable = convertAcc(tree, Nil)


def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val table = convert(tree)
  text match {
    case Nil => Nil
    case c::cs => codeBits(table)(c) ++ quickEncode(tree)(cs)
  }
}








val toto = "helloworld"
times(string2Chars(toto))
makeOrderedLeafList(times(string2Chars(toto)))
combine(makeOrderedLeafList(times(string2Chars(toto))))
combine(makeOrderedLeafList(times(string2Chars(toto)))).length
combine(combine(makeOrderedLeafList(times(string2Chars(toto)))))
combine(combine(makeOrderedLeafList(times(string2Chars(toto))))).length
combine(combine(combine(makeOrderedLeafList(times(string2Chars(toto))))))
combine(combine(combine(makeOrderedLeafList(times(string2Chars(toto)))))).length
combine(combine(combine(combine(makeOrderedLeafList(times(string2Chars(toto)))))))
combine(combine(combine(combine(makeOrderedLeafList(times(string2Chars(toto))))))).length
combine(combine(combine(combine(combine(makeOrderedLeafList(times(string2Chars(toto))))))))
combine(combine(combine(combine(combine(makeOrderedLeafList(times(string2Chars(toto)))))))).length
combine(combine(combine(combine(combine(combine(makeOrderedLeafList(times(string2Chars(toto)))))))))
combine(combine(combine(combine(combine(combine(makeOrderedLeafList(times(string2Chars(toto))))))))).length
println("*******")
createCodeTree(string2Chars(toto))
//val tata = makeOrderedLeafList(times(string2Chars(toto)))
//singleton(tata)

val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

/**
 * What does the secret message say? Can you decode it?
 * For the decoding use the `frenchCode' Huffman tree defined above.
 */
val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)


def decodedSecret = decode(frenchCode, secret)

val helloCodeTree = createCodeTree(string2Chars(toto))
val helloBits: List[Bit] = List(0,0,0,0,0,1)

val helloString = decode(helloCodeTree, helloBits)
println(helloString)

findLeafBits(frenchCode, 'a', Nil)
findLeafBits(frenchCode, 'b', Nil)
findLeafBits(frenchCode, 'a', Nil) ++ findLeafBits(frenchCode, 'b', Nil)
encode(frenchCode)(List('a','b'))

convert(frenchCode)
findLeafBits(frenchCode, 's', Nil)
findLeafBits(frenchCode, 'd', Nil)
findLeafBits(frenchCode, 'x', Nil)



encode(frenchCode)(decodedSecret)
quickEncode(frenchCode)(decodedSecret)