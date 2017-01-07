package org.pfcoperez.dailyalgorithm

import org.pfcoperez.dailyalgorithm.Algebra.Matrix.{Matrix, positionalValues}

import scala.collection.immutable.Queue

object Graphs {

  type NoWeight = Unit
  type Arch[T, W] = (W, Node[T, W])
  type DirectedGraph[T] = DirectedWeighedGraph[T, NoWeight]

  trait DirectedWeighedGraph[T, W]

  object WeighedNode {
    def apply[T, W](x: T, archs: Seq[(W, Node[T, W])]): Node[T, W] =
      new Node(x, archs.toList)
  }

  object Node {
    def apply[T](x: T, children: Seq[Node[T, NoWeight]] = Seq.empty): Node[T, NoWeight] =
      new Node(x, children.view.map(child => ((), child)).toList)

    def unapply[T](arg: Node[T, NoWeight]): Option[(T, Seq[Node[T, NoWeight]])] =
      Some(arg.value -> arg.children)
  }

  class Node[T, W](val value: T, val archs: List[Arch[T, W]]) extends DirectedWeighedGraph[T, W] {
    def children: Seq[Node[T, W]] = archs.view map { case (_, node: Node[T, W]) => node }
  }
  case class Empty[T, W]() extends DirectedWeighedGraph[T, W]


  implicit def arch2node[T, W](arch: Arch[T, W]): Node[T, W] = arch._2

  /**
    * O(n), n = number of nodes
    */
  def breadthFirstTake[T, S](g: DirectedGraph[T])(f: PartialFunction[T, S]): List[S] = {

    def traverseRec(toVisit: Queue[DirectedGraph[T]], acc: List[S]): List[S] = toVisit.headOption collect {
      case Node(value, children) =>
        val (newToVisit, toAcc) = f.lift(value) map { res =>
          (toVisit.tail ++ children, Some(res))
        } getOrElse(toVisit.tail -> None)
        traverseRec(newToVisit, toAcc map (_::acc) getOrElse acc)
    } getOrElse acc.reverse

    traverseRec(Queue(g), Nil)

  }

  /**
    * O(n), n = number of nodes
    *
    * Use example:
    *
    * val g: DirectedGraph[Int] =
    *   Node(1, Node(2, Node(5, Nil)::Nil):: Node(3, Node(6, Node(7, Nil)::Nil)::Nil)::Node(4, Nil)::Nil)
    *
    * depthFirstTake(g) { case x => x }
    *
    */
  def depthFirstTake[T, S](g: DirectedGraph[T])(f: PartialFunction[T, S]): List[S] = {

    type Stack[T] = List[T]

    def traverseRec(toVisit: Stack[DirectedGraph[T]], acc: List[S]): List[S] = toVisit.headOption collect {
      case Node(value, children) =>
        val (newToVisit: Stack[Node[T, NoWeight]], toAcc) = f.lift(value) map { res =>
          (children ++ toVisit.tail, Some(res))
        } getOrElse(toVisit.tail -> None)
        traverseRec(newToVisit, toAcc map (_::acc) getOrElse acc)
    } getOrElse acc.reverse

    traverseRec(g::Nil, Nil)

  }

  /**
    * O(n)*T(f)
    */
  def breadthFirstFold[T,S](z: S, g: DirectedGraph[T])(f: PartialFunction[(S, T), S]): S = {

    def traverseRec(toVisit: Queue[DirectedGraph[T]], acc: S): S =
      toVisit.headOption collect {
        case Node(value, children) =>
          val (newToVisit, newAcc) = f.lift(acc, value) map { res =>
            (toVisit.tail ++ children, res)
          } getOrElse(toVisit.tail -> acc)
          traverseRec(newToVisit, newAcc)
      } getOrElse acc

    traverseRec(Queue(g), z)

  }

  /**
    * Adjacency matrix from Directed graph, based on the position of the node within it, not on its value.
    * O(n), n = Number of nodes
    *
    * val g: Node[Int] =
    *          Node(1, Node(2, Node(5, Nil)::Nil):: Node(3, Node(6, Node(7, Nil)::Nil)::Nil)::Node(4, Nil)::Nil)
    *
    * val m: Matrix[Boolean] = g
    */
  implicit def graph2matrix[T](g: DirectedGraph[T]): Matrix[Boolean] = {

    type AdjacencyMap = Map[Node[T, NoWeight], Set[Node[T, NoWeight]]]

    def adjacencyMap(
                      toVisit: Queue[DirectedGraph[T]],
                      acc: AdjacencyMap,
                      breadCrumbs: List[Node[T, NoWeight]],
                      visited: Set[Node[T, NoWeight]]
                    ): (AdjacencyMap, List[Node[T, NoWeight]]) =
      toVisit.headOption collect {
        case node @ Node(_, children) =>
          adjacencyMap(
            toVisit.tail ++ children.filterNot(acc contains _),
            acc + (node -> children.toSet),
            if(!visited.contains(node)) node::breadCrumbs else breadCrumbs,
            visited + node
          )
        case Empty() => adjacencyMap(toVisit.tail, acc, breadCrumbs, visited)
      } getOrElse (acc, breadCrumbs reverse)

    val (node2adjacents, nodes) = adjacencyMap(Queue(g), Map.empty, Nil, Set.empty)
    val pos2node = nodes toArray

    positionalValues[Boolean](pos2node.length, pos2node.length) {
      case (i, j) => node2adjacents get(pos2node(i)) map(_.contains(pos2node(j))) getOrElse false
    }

  }

  def atSameConnectedComponent[T, W](a: Node[T, W], b: Node[T, W]): Boolean = {
    import scala.collection.immutable.Queue
    case class ExplorationState(
                                 lastVisited: Option[Node[T, W]],
                                 visited: Set[Node[T,W]],
                                 toVisit: Queue[Node[T, W]]
                               )

    def bidirectionalSearch(
                             fromA: ExplorationState,
                             fromB: ExplorationState
                           ): Boolean = {
      def explorationStep(st: ExplorationState, goal: Node[T, W]): ExplorationState =
        st.toVisit.headOption map { currentNode =>
          if(currentNode.children contains goal) st
          else {
            ExplorationState(
              Some(currentNode),
              st.visited + currentNode,
              st.toVisit.tail ++ currentNode.children.filterNot(st.visited contains _)
            )
          }
        } getOrElse st

      val newFromA @ ExplorationState(Some(lastFromA), visitedFromA, _) = explorationStep(fromA, b)
      val newFromB @ ExplorationState(Some(lastFromB), visitedFromB, _) = explorationStep(fromB, b)

      val touchedFromA = visitedFromB contains lastFromA
      val touchedFromB = visitedFromA contains lastFromB

      val changedState = (newFromA != fromA) || (newFromB != fromB)

      touchedFromA || touchedFromB || (changedState && bidirectionalSearch(newFromA, newFromB))

    }

    bidirectionalSearch(
      ExplorationState(None, Set.empty, Queue(a)),
      ExplorationState(None, Set.empty, Queue(b))
    )

  }

  def minDistancesDijkstra[T, W](root: Node[T, W])(
    implicit weightNumeric: Numeric[W]): Map[Node[T,W], W] = {

    import weightNumeric.{mkNumericOps, mkOrderingOps}

    type NodeSet = Set[Node[T, W]]
    type Distances = Map[Node[T, W], W]

    def dijkstraIteration(visited: NodeSet, distances: Distances): (NodeSet, Distances) = {
      val toVisitDistances = distances.filter(entry => !visited.contains(entry._1))
      if(toVisitDistances.nonEmpty) {
        val (selectedNode, d) = toVisitDistances.minBy(_._2)
        val newDistances = (distances /: selectedNode.archs) {
          case (prevDistances, (delta: W, goalNode)) =>
            val newDistanceValue = prevDistances.get(goalNode) collect {
              case prevDistance: W if prevDistance <= d + delta => prevDistance
            } getOrElse (d + delta)
            prevDistances.updated(goalNode,  newDistanceValue)
          }
        dijkstraIteration(visited + selectedNode, newDistances)
      } else (visited, distances)

    }

    dijkstraIteration(Set.empty, Map(root -> weightNumeric.zero))._2

  }

  object LinkedStructures {

    object LinkedList {
      // List factory: O(n)
      def apply[T](elements: T*): LinkedList[T] =
        (elements :\ (Empty: LinkedList[T])) {
          (v, prev) => v +: prev
        }
    }

    //Algebraic data type LinkedList = Empty | Element x
    trait LinkedList[+T] {
      // Prepend value: O(1)
      def +:[S >: T](x: S): LinkedList[S] = Element(x, this)
      def length: Int
      def next: LinkedList[T]
    }

    case class Element[+T](x: T, val next: LinkedList[T]) extends LinkedList[T] {

      def length: Int = {
        def tailRecLength(l: LinkedList[T], acc: Int = 0): Int = l match {
          case Element(_, next) => tailRecLength(next, acc+1)
          case _ => acc
        }
        tailRecLength(this)
      }

    }

    case object Empty extends LinkedList[Nothing] {
      override def length: Int = 0
      override def next: LinkedList[Nothing] =
        throw new RuntimeException("Serial crash: Asking EMPTY for next")
    }

  }

  object BinaryTrees {

    trait BinaryTree[+T]
    case class Node[T, NoWeight](left: BinaryTree[T], v: T, right: BinaryTree[T]) extends BinaryTree[T]
    case object Empty extends BinaryTree[Nothing]

    trait BinaryTreeOps {
      def min[T : Ordering](btree: BinaryTree[T]): Option[T]
      def max[T : Ordering](btree: BinaryTree[T]): Option[T]
      def findFirst[T : Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T]
      def toList[T : Ordering](btree: BinaryTree[T]): List[T]
      def insert[T : Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T]
      def delete[T: Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T]
      def height[T](binaryTree: BinaryTree[T], limit: Option[Int] = None): Int
    }

    object RawBinaryTree extends BinaryTreeOps {

      // O(h), h = tree height
      def partialFold[T : Ordering, S](btree: BinaryTree[T], acc: S)(
        pathStep: PartialFunction[(BinaryTree[T], S), (BinaryTree[T], S)]): S =
        pathStep.lift(btree -> acc) map { case (t, a) => partialFold(t, a)(pathStep) } getOrElse(acc)

      // O(h), h = tree height
      def min[T : Ordering](btree: BinaryTree[T]): Option[T] = partialFold(btree, Option.empty[T]) {
        case (Node(left, v, _), _) => left -> Some(v)
      }

      // O(h), h = tree height
      def max[T : Ordering](btree: BinaryTree[T]): Option[T] = partialFold(btree, Option.empty[T]) {
        case (Node(_, v, right), _) => right -> Some(v)
      }

      // O(h), h = tree height
      def findFirst[T](btree: BinaryTree[T])(v: T)(implicit order: Ordering[T]): BinaryTree[T] =
        partialFold(btree, Empty : BinaryTree[T]) {
          case (node @ Node(left, nodeval, right), _) =>
            import order.mkOrderingOps
            if(nodeval == v) Empty -> node
            else if(v < nodeval) left -> Empty
            else right -> Empty
          case _ => Empty -> Empty
        }

      // O(h), h = tree height
      def height[T](binaryTree: BinaryTree[T], limit: Option[Int] = None): Int = {
        def hrec(btree: BinaryTree[T], acc: Int): Int =
          btree match {
            case Empty => acc
            case _ if limit.map(_ == acc).getOrElse(false) => acc
            case Node(left, _, right) => math.max(hrec(left,acc+1), hrec(right, acc+1))
          }
        hrec(binaryTree, 0)
      }

      // O(h), h = tree height
      def insert[T](btree: BinaryTree[T])(v: T)(
        implicit order: Ordering[T]
      ): BinaryTree[T] = btree match {
        case Empty => Node(Empty, v, Empty)
        case node @ Node(left, nodeval, right) =>
          import order.mkOrderingOps
          if(nodeval == v) node
          else if(v < nodeval) Node(insert(left)(v), nodeval, right)
          else Node(left, nodeval, insert(right)(v))
      }

      // O(h), h = tree height
      def insertNode[T, NoWeight](btree: BinaryTree[T])(node: Node[T, NoWeight])(
        implicit order: Ordering[T]
      ): BinaryTree[T] = btree match {
        case Empty => node
        case Node(left, nodeval, right) =>
          import order.mkOrderingOps
          if(node.v <= nodeval) Node(insertNode(left)(node), nodeval, right)
          else Node(left, nodeval, insertNode(right)(node))
      }

      def toList[T : Ordering](btree: BinaryTree[T]): List[T] = btree match {
        case Node(left, v, right) => toList(left) ++ List(v) ++ toList(right)
        case _ => Nil
      }

      // O(h), h = tree height
      def delete[T](btree: BinaryTree[T])(v: T)(
        implicit order: Ordering[T]
      ): BinaryTree[T] =
        btree match {
          case Empty => Empty
          case n @ Node(left, nodeval, right) =>
            import order.mkOrderingOps
            if(nodeval == v)
              (left, right) match {
                case (Node(_, _, leftsRight), Node(rightsLeft, _, _)) =>
                  val Seq(target, source) = Seq(left -> leftsRight, right -> rightsLeft) sortBy {
                    case (_, toInsert) => height(toInsert)
                  } map (_._1)
                  source match {
                    case Empty => target
                    case source: Node[T, NoWeight] => insertNode(target)(source)
                  }
                case (n: Node[T, NoWeight], Empty) => n
                case (Empty, n: Node[T, NoWeight]) => n
                case _ => Empty
              }
            else if(v < nodeval)
              Node(delete(left)(v), nodeval, right)
            else
              Node(left, nodeval, delete(right)(v))
        }

    }

    object BalancedBinaryTree extends BinaryTreeOps {
      def min[T: Ordering](btree: BinaryTree[T]): Option[T] = RawBinaryTree.min(btree)
      def max[T: Ordering](btree: BinaryTree[T]): Option[T] = RawBinaryTree.max(btree)

      def findFirst[T: Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T] =
        RawBinaryTree.findFirst(btree)(v)

      def toList[T: Ordering](btree: BinaryTree[T]): List[T] = RawBinaryTree.toList(btree)

      def height[T](binaryTree: BinaryTree[T], limit: Option[Int] = None): Int =
        RawBinaryTree.height(binaryTree, limit)

      private def leftRotation[T]: PartialFunction[Node[T, NoWeight], Node[T, NoWeight]] = {
        case Node(left, value, Node(rightsLeft, rightsValue, rightsRight)) =>
          Node(Node(left, value, rightsLeft), rightsValue, rightsRight)
        case other => other
      }

      private def rightRotation[T]: PartialFunction[Node[T, NoWeight], Node[T, NoWeight]] = {
        case Node(Node(leftsLeft, leftsValue, leftsRight), value, right) =>
          Node(leftsLeft, leftsValue, Node(leftsRight, value, right))
        case other => other
      }

      def insert[T](btree: BinaryTree[T])(v: T)(
        implicit order: Ordering[T]
      ): BinaryTree[T] = {
        import order.mkOrderingOps

        def balancedInsert(btree: BinaryTree[T]): (BinaryTree[T], Option[Int]) = {

          def createBalancedSubtree(
                                     targetBranch: BinaryTree[T],
                                     secondBranch: BinaryTree[T],
                                     rotation: PartialFunction[Node[T, NoWeight], Node[T, NoWeight]])(
            nodeBuilder: (BinaryTree[T], BinaryTree[T]) => Node[T, NoWeight]
          ): (BinaryTree[T], Option[Int]) = {
            val (updatedBranch, heightTrack) = balancedInsert(targetBranch)
            val h2propagate = heightTrack flatMap { h =>
              val othersh = height(secondBranch, Some(h+2))
              if(h - othersh > 1) None
              else Some(math.max(h, othersh)+1)
            }
            val newNode = nodeBuilder(updatedBranch, secondBranch)
            (if(heightTrack.isEmpty != h2propagate.isEmpty) rotation(newNode) else newNode, h2propagate)

          }

          btree match {
            case Empty => Node(Empty, v, Empty) -> Some(0)
            case node @ Node(left, nodeval, right) =>
              if(nodeval == v) node -> Some(height(node))
              else if(v < nodeval)
                createBalancedSubtree(left, right, rightRotation) {
                  Node(_, nodeval, _)
                }
              else
                createBalancedSubtree(right, left, leftRotation) {
                  (targetBranch, secondBranch) => Node(secondBranch, nodeval, targetBranch)
                }
          }
        }

        balancedInsert(btree)._1

      }

      override def delete[T: Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T] = ???

    }

  }

  object Tries {

    case class TrieNode[KE, V](v: Option[V], children: Map[KE, TrieNode[KE, V]]) {

      /**
        * Returns a new Trie built upon 'this' which contains (k, newVal)
        *
        * O(n), n = length of 'k'
        */
      def insert(k: Seq[KE])(newVal: V): TrieNode[KE, V] = k match {
        case Seq() => copy(v = Some(newVal))
        case _ =>
          val keyElement = k.head
          val newChild = keyElement -> children.getOrElse(keyElement, TrieNode[KE, V](None, Map.empty)).insert(k.tail)(newVal)
          copy(children = children + newChild)
      }

      /**
        * Look for the key 'k' in the trie.
        * @param k
        * @return The Some(value) associated to the key in the Trie or None if not found
        *
        * O(n), n = length of 'k'
        */
      def find(k: Seq[KE]): Option[V] = k.headOption flatMap { keyElement =>
        for(child <- children.get(keyElement); res <- child.find(k.tail)) yield res
      } orElse v

      def contains(k: Seq[KE]): Boolean = find(k).isDefined

    }

  }


}
