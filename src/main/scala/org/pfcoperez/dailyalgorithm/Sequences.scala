package org.pfcoperez.dailyalgorithm

import scala.math.Ordering
import org.pfcoperez.dailyalgorithm.datastructures.Bits
import org.pfcoperez.dailyalgorithm.datastructures.Bits.{ Error => BitsError }

object Sequences {

  /**
   * Palindromator: Shortest palindrome from a given string
   *
   * O(2^n)
   *
   */
  def shortestPalindromeFrom(s: String): String = {

    def recGen(ss: String, sr: String, acc: (String, String)): String = {
      if (ss.length + sr.length <= 1) s"${acc._1}${ss + sr}${acc._2}"
      else if (ss.isEmpty) recGen(sr.init, sr, acc)
      else if (sr.isEmpty) recGen(ss, ss.init, acc)
      else if (ss.head == sr.head) {
        val v = ss.head
        recGen(ss.tail, sr.tail, (s"${acc._1}$v", s"$v${acc._2}"))
      } else Seq(recGen(sr.head + ss, sr, acc), recGen(ss, ss.head + sr, acc)) minBy (_.length)
    }

    val (left, right) = s splitAt (s.length / 2)

    recGen(left, right reverse, ("", ""))

  }

  /**
   * Booth's lexicographic minimal rotation
   * O(n)
   *
   * Provides the rotation index leading to a minimum lexicographic order
   * & keeping the in-sequence order.
   */
  def minimalLexicographicRotation[T](s: Vector[T])(implicit ordEvidence: Ordering[T]): Int = {

    implicit class CyclicVector[S](val v: Vector[S]) {
      private def safeIndex(idx: Int): Int =
        (if (idx >= 0) idx else (math.abs(idx) / v.size + 1) * v.size + idx) % v.size
      def apply(idx: Int): S = v(safeIndex(idx))
      def update(idx: Int, value: S): CyclicVector[S] = v.updated(safeIndex(idx), value)
    }

    val cs: CyclicVector[T] = s

    import ordEvidence.mkOrderingOps

    def recMinRot(f: CyclicVector[Int], k0: Int, j: Int): Int =
      if (j == s.size * 2) k0 else {

        def findIK(i: Int, k: Int): (Int, Int) =
          if (i == -1 || cs(j) == cs(k + i + 1)) i -> k
          else findIK(f(i), if (cs(j) < cs(k + i + 1)) j - i - 1 else k)

        val (i, k) = findIK(f(j - k0 - 1), k0)

        val (newk, newFjk) =
          if (cs(j) != cs(k + i + 1)) (if (cs(j) < cs(k)) j else k, -1)
          else (k, i + 1)

        recMinRot(f.update(j - k, newFjk), newk, j + 1)
      }

    recMinRot(Vector.fill(s.size)(-1), 0, 1)
  }

  /**
   * Checks whether a sequence if a subsequence of another
   *
   * O(n*m), n = size of s, m = size of ss
   */
  def isSubseq[T](s: Seq[T])(ss: Seq[T]): Boolean =
    (ss /: s) {
      case (Seq(), _) => Seq()
      case (remaining, x) if (remaining.head == x) => remaining.tail
      case _ => ss
    } isEmpty

  /**
   * Search for an element within an cyclic-ordered vector
   * returning its position if found.
   * O(log n)
   */
  def binarySearchInCycle[T](v: Vector[T])(x: T)(
    implicit
    ordering: Ordering[T], i: Int = 0, j: Int = v.size - 1): Option[Int] = {
    import ordering.mkOrderingOps

    val l = j - i + 1
    val midIdx = l / 2

    if (i > j) None
    else if (v(midIdx) == x) Some(midIdx)
    else {
      type SearchFunction = (Int, Int) => Option[Int]

      val searchFunctions: (SearchFunction, SearchFunction) =
        (binarySearch(v)(x)(ordering, _, _), binarySearchInCycle(v)(x)(ordering, _, _))

      if (v(i) <= v(midIdx)) // Left side ordered
        if (v(i) >= x && x < v(midIdx)) searchFunctions._1(i, midIdx - 1)
        else searchFunctions._2(midIdx + 1, j)
      else // Right side ordered
      if (v(midIdx) < x && x <= v(j)) searchFunctions._1(midIdx + 1, j)
      else searchFunctions._2(i, midIdx - 1)

    }

  }

  /**
   * Search for an element within an ordered vector returning its position if found.
   * O(log n)
   */
  def binarySearch[T](v: Vector[T])(x: T)(
    implicit
    ordering: Ordering[T], i: Int = 0, j: Int = v.size - 1): Option[Int] = {
    import ordering.mkOrderingOps

    val l = j - i + 1
    val midIdx = i + l / 2

    if (i > j) None
    else if (x == v(midIdx)) Some(midIdx)
    else if (x < v(midIdx)) binarySearch(v)(x)(ordering, i, midIdx - 1)
    else binarySearch(v)(x)(ordering, midIdx + 1, j)
  }

  def rotateLeft[T](v: Vector[T])(idx: Int): Vector[T] = { val (a, b) = v.splitAt(idx); b ++ a }

  /**
   *
   * Knuth-Morris-Pratt fast sequence matching.
   * O(n), n = Size of the corpus where the target word is to be seached in.
   *
   */
  def kmpSearch[T](corpus: Seq[T], word: Seq[T]): Seq[Int] = {

    val l = word.size
    val wordVector = word.toVector

    def findOffset(v: T, offset: Int, prefixTable: Vector[Int]): Int =
      if (offset > -1 && v != wordVector(offset + 1)) findOffset(v, prefixTable(offset), prefixTable)
      else offset

    val (_, prefixTable) =
      ((-1, Vector.fill(l)(-1)) /: (word.view.zipWithIndex).tail) {
        case ((k, p), (v, q)) =>
          val newK = {
            val kv = findOffset(v, k, p)
            kv + { if (wordVector(kv + 1) == v) 1 else 0 }
          }
          (newK, p.updated(q, newK))
      }

    val (_, results) = ((-1, List.empty[Int]) /: corpus.view.zipWithIndex) {
      case ((q, results), (v, idx)) =>
        val newQ = {
          val qv = findOffset(v, q, prefixTable)
          qv + { if (wordVector(qv + 1) == v) 1 else 0 }
        }
        if (newQ == l - 1) (prefixTable(newQ), (idx - l + 1) :: results)
        else (newQ, results)
    }

    results.reverse
  }

  /**
   * Find the longest consecutive chain of elements from `source`.
   * O(n), n = number of elements in `source`.
   *
   */
  def maxConsecutiveSequence[T](source: Seq[T])(implicit ord: Numeric[T]): Seq[T] = {
    import ord.{ mkNumericOps, one }

    def maxConsecutiveSequence(available: Set[T], maxSeq: Vector[T], current: Vector[T]): Seq[T] =
      if (available.isEmpty) maxSeq.toList else {

        val (nextSeq, nextAvailable) = {
          if (current.isEmpty) {
            (Vector(available.head), available.tail)
          } else if (available.contains(current.head - one)) {
            val candidate = current.head - one
            (candidate +: current, available - candidate)
          } else if (available.contains(current.last + one)) {
            val candidate = current.last + one
            (current :+ candidate, available - candidate)
          } else
            (Vector.empty, available)
        }

        maxConsecutiveSequence(nextAvailable, Seq(maxSeq, nextSeq).maxBy(_.size), nextSeq)
      }

    maxConsecutiveSequence(source.toSet, Vector(), Vector())
  }

  object HuffmanCoding {

    import datastructures.graphs.directed.trees.binary.{ BinaryTree, Node, Empty }
    import datastructures.heaps.BinaryHeap

    /**
     * Context class gluing a table to encode symbols and a binary to translate
     * binary encodings back to the symbol they represent.
     *
     */
    case class AlphabetCode[T](encodingTable: Map[T, Bits], decodingTree: BinaryTree[Option[T]])

    /**
     * Build the required table to translate symbols into binary codes
     * as well as the tree required to go back from binary encoding to
     * the symbol generating it.
     *
     * O(n*log(n)), n = number of symbols
     *
     * @param alphabet frequency table: Statistics on the language.
     */
    def generateAlphabetCode[T](alphabet: Map[T, Int]): AlphabetCode[T] = {
      // The algorithm start with a forest of binary trees...

      type SymbolNode = Node[Option[T]]
      type SymbolTree = (SymbolNode, BigInt)

      val emptyForest = BinaryHeap.empty[SymbolTree](Ordering.by((t: SymbolTree) => t._2))

      // ... one single element tree per alphabet symbol.
      val primordialForest = (emptyForest /: alphabet) {
        case (forest, (symbol, frequency)) =>
          forest enqueue Node(Empty, Option(symbol), Empty) -> BigInt(frequency)
      }

      /**
       * Cluster symbols by pairs: Two less frequent symbols become a parent node.
       * O(n*log(n)), n = number of symbols
       */
      def buildSymbolsTree(forest: BinaryHeap[SymbolTree]): BinaryTree[Option[T]] =
        if (forest.isEmpty) Empty
        else if (forest.size == 1) forest.head._1
        else {
          val (nodeA, fA) = forest.head
          val forestTail = forest.dequeue
          val (nodeB, fB) = forestTail.head

          val newTree = (Node(nodeA, None, nodeB), fA + fB)

          buildSymbolsTree(forestTail.dequeue.enqueue(newTree))
        }

      // The initial forest is then clustered by pairing less frequent symbols
      val symbolsTree = buildSymbolsTree(primordialForest)

      import cats.syntax.either._

      /**
       * The tree itself can be used to translate
       * binary strings into symbols (each bit, a decision: left/right)
       */

      /**
       * Use the encoding to symbol translation tree to build
       * the symbol to encoding table.
       * O(n), n = number of symbols
       */
      def buildTable(pathAndTreeToExplore: List[(Bits, BinaryTree[Option[T]])], acc: Map[T, Bits]): Map[T, Bits] =
        pathAndTreeToExplore match {
          case (_, Empty) :: remaining => buildTable(remaining, acc)
          case (path, Node(leftNode, v, rightNode)) :: remaining =>
            val newAccumulator = v map (symbol => acc + (symbol -> path)) getOrElse acc
            val Seq(left, right) = Seq(leftNode, rightNode).zipWithIndex flatMap {
              case (node, bit) => path.append(bit == 1).toOption.map(_ -> node)
            }
            buildTable(left :: right :: remaining, newAccumulator)
          case _ => acc
        }

      AlphabetCode(buildTable((Bits.empty, symbolsTree) :: Nil, Map.empty), symbolsTree)

    }

    import cats.syntax.monoid._
    import cats.syntax.either._
    import cats.syntax.foldable._
    import cats.instances.list._

    import Bits.bitsMonoid

    /**
     * Encode a message as an optimized bit array using Huffman coding
     * O(n), n = input message size
     */
    def encodeMessage[T](msg: List[T])(implicit alphabetCode: AlphabetCode[T]): Bits =
      msg.foldMap(alphabetCode.encodingTable)

    /**
     * Decode the bits within an optimized bit array into a message (sequence of symbols)
     * O(n), n = output message size
     */
    def decodeMessage[T](binary: Bits)(implicit alphabetCode: AlphabetCode[T]): Either[BitsError, List[T]] = {
      import alphabetCode.decodingTree
      val L = binary.length

      def decode(
        idx: Long,
        acc: Either[BitsError, List[T]],
        currentNode: BinaryTree[Option[T]]): Either[BitsError, List[T]] = {
        val ebit = binary(idx)
        if (idx == L || ebit.isRight) {
          lazy val Right(bit) = ebit
          (idx, currentNode) match {
            case (L, Node(_, Some(symbol), _)) => acc.map(symbol :: _)
            case (L, _) => acc
            case (_, Node(Empty, Some(symbol), _)) if !binary(idx).right.get =>
              decode(idx, acc.map(symbol :: _), decodingTree)
            case (_, Node(_, Some(symbol), Empty)) if binary(idx).right.get =>
              decode(idx, acc.map(symbol :: _), decodingTree)
            case (_, Node(left, _, right)) =>
              decode(idx + 1L, acc, if (binary(idx).right.get) right else left)
          }
        } else ebit.map(_ => Nil)
      }

      decode(0, Nil.asRight[BitsError], decodingTree).map(_.reverse)
    }

  }

}
