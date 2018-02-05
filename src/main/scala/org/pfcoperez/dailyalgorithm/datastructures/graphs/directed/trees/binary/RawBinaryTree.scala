package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees.binary

trait RawBinaryTreeOps extends BinaryTreeOps {

  // O(h), h = tree height
  def partialFold[T: Ordering, S](btree: BinaryTree[T], acc: S)(
    pathStep: PartialFunction[(BinaryTree[T], S), (BinaryTree[T], S)]): S =
    pathStep.lift(btree -> acc) map { case (t, a) => partialFold(t, a)(pathStep) } getOrElse (acc)

  // O(h), h = tree height
  def min[T: Ordering](btree: BinaryTree[T]): Option[T] = partialFold(btree, Option.empty[T]) {
    case (Node(left, v, _), _) => left -> Some(v)
  }

  // O(h), h = tree height
  def max[T: Ordering](btree: BinaryTree[T]): Option[T] = partialFold(btree, Option.empty[T]) {
    case (Node(_, v, right), _) => right -> Some(v)
  }

  // O(h), h = tree height
  def findFirst[T](btree: BinaryTree[T])(v: T)(implicit order: Ordering[T]): BinaryTree[T] =
    partialFold(btree, Empty: BinaryTree[T]) {
      case (node @ Node(left, nodeval, right), _) =>
        import order.mkOrderingOps
        if (nodeval == v) Empty -> node
        else if (v < nodeval) left -> Empty
        else right -> Empty
      case _ => Empty -> Empty
    }

  // O(h), h = tree height
  def height[T](binaryTree: BinaryTree[T], limit: Option[Int] = None): Int = {
    def hrec(btree: BinaryTree[T], acc: Int): Int =
      btree match {
        case Empty => acc
        case _ if limit.map(_ == acc).getOrElse(false) => acc
        case Node(left, _, right) => math.max(hrec(left, acc + 1), hrec(right, acc + 1))
      }
    hrec(binaryTree, 0)
  }

  import cats.Eval

  // O(h), h = tree height
  def insert[T](btree: BinaryTree[T])(v: T)(
    implicit
    order: Ordering[T]): BinaryTree[T] = {
    def insert(btree: BinaryTree[T])(v: T): Eval[BinaryTree[T]] =
      btree match {
        case Empty => Eval.now(Node(Empty, v, Empty))
        case node @ Node(left, nodeval, right) =>
          import order.mkOrderingOps
          if (nodeval == v) Eval.now(node)
          else if (v < nodeval) Eval.defer(insert(left)(v)).map(Node(_, nodeval, right))
          else Eval.defer(insert(right)(v)).map(Node(left, nodeval, _))
      }
    insert(btree)(v).value
  }

  // O(h), h = tree height
  def insertNode[T, NoWeight](btree: BinaryTree[T])(node: Node[T])(
    implicit
    order: Ordering[T]): BinaryTree[T] = btree match {
    case Empty => node
    case Node(left, nodeval, right) =>
      import order.mkOrderingOps
      if (node.v <= nodeval) Node(insertNode(left)(node), nodeval, right)
      else Node(left, nodeval, insertNode(right)(node))
  }

  // O(h), h = tree height
  def delete[T](btree: BinaryTree[T])(v: T)(
    implicit
    order: Ordering[T]): BinaryTree[T] =
    btree match {
      case Empty => Empty
      case n @ Node(left, nodeval, right) =>
        import order.mkOrderingOps
        if (nodeval == v)
          (left, right) match {
            case (Node(_, _, leftsRight), Node(rightsLeft, _, _)) =>
              val Seq(target, source) = Seq(left -> leftsRight, right -> rightsLeft) sortBy {
                case (_, toInsert) => height(toInsert)
              } map (_._1)
              source match {
                case Empty => target
                case source: Node[T] => insertNode(target)(source)
              }
            case (n: Node[T], Empty) => n
            case (Empty, n: Node[T]) => n
            case _ => Empty
          }
        else if (v < nodeval)
          Node(delete(left)(v), nodeval, right)
        else
          Node(left, nodeval, delete(right)(v))
    }

  def toList[T: Ordering](btree: BinaryTree[T]): List[T] = {

    def toList(btree: BinaryTree[T])(acc: List[T]): List[T] = btree match {
      case Node(left, v, right) => toList(left)(v :: toList(right)(acc))
      case _ => acc
    }

    toList(btree)(Nil)

  }

  /**
   * Create a new binary tree by combining each node value
   * with its heigth in the tree.
   *
   * O(n), n=number of nodes
   *
   * @param binaryTree Source binary tree
   * @return New binary tree containing the same values paired
   *         with each node height.
   */
  def zipWithHeight[T](binaryTree: BinaryTree[T]): BinaryTree[(T, Int)] =
    binaryTree match {
      case Node(left, v, right) =>
        val newChildren @ Seq(newLeft, newRight) = Seq(left, right) map zipWithHeight
        val h = {
          newChildren map {
            case Empty => 1
            case Node(_, (_, h), _) => h + 1
          }
        }.max
        Node(newLeft, v -> h, newRight)
      case _ => Empty
    }

  def map[T, S](btree: BinaryTree[T])(f: T => S): BinaryTree[S] = btree match {
    case Node(left, v, right) => Node(map(left)(f), f(v), map(right)(f))
    case _ => Empty
  }

  /**
   * Merge two binary trees. If they are balanced, the resulting tree
   * will be balanced too.
   *
   * @param a Binary tree
   * @param b Binary tree
   * @return Merged binary tree keeping balance if both input trees
   *         were balanced.
   */
  def merge[T](a: BinaryTree[T], b: BinaryTree[T])(implicit ord: Ordering[T]): BinaryTree[T] = (a, b) match {

    case (Empty, _) => b
    case (_, Empty) => a
    case _ =>

      val sources @ Seq(la, lb) = Seq(a, b) map toList[T]

      val maxValue = (sources map (_.max)) max

      def consumeAndMerge(la: List[T], lb: List[T])(acc: BinaryTree[T]): BinaryTree[T] = {
        Seq(la, lb).sortBy(_.headOption.getOrElse(maxValue)) match {
          case Seq(av :: arem, b) =>
            consumeAndMerge(arem, b)(insert(acc)(av))
          case _ => acc
        }
      }

      consumeAndMerge(la, lb)(Empty)

  }

}

object RawBinaryTree extends RawBinaryTreeOps

