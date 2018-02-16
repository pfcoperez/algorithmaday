package org.pfcoperez.dailyalgorithm.datastructures.graphs

package object directed {

  import org.pfcoperez.dailyalgorithm.Algebra.Matrix
  import org.pfcoperez.dailyalgorithm.Algebra.Matrix.positionalValues

  import scala.collection.immutable.Queue

  type NoWeight = Unit
  type Arch[T, W] = (W, Node[T, W])
  type DirectedGraph[T] = DirectedWeighedGraph[T, NoWeight]

  trait DirectedWeighedGraph[+T, +W]

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

  class Node[+T, +W](val value: T, val archs: List[Arch[T, W]]) extends DirectedWeighedGraph[T, W] {
    def children: Seq[Node[T, W]] = archs.view map { case (_, node: Node[T, W]) => node }
  }
  case class Empty() extends DirectedWeighedGraph[Nothing, Nothing]

  implicit def arch2node[T, W](arch: Arch[T, W]): Node[T, W] = arch._2

  /**
   * Map function to build a new map from an existing one
   * allowing node value modifications as well as arches' weights.
   *
   * O(n + m), n = number of nodes in the graph, m = number of arches in the graph
   *
   */
  def map[T, W, NT, NW](dg: DirectedWeighedGraph[T, W])(
    fValues: T => NT,
    fWeights: W => NW): DirectedWeighedGraph[NT, NW] = {

    def mapImp(
      n: Node[T, W],
      visited: Map[Node[T, W], Node[NT, NW]] = Map.empty): (Node[NT, NW], Map[Node[T, W], Node[NT, NW]]) =

      visited.get(n).map(_ -> visited) getOrElse {
        val (newArchs, newVisited) = ((List.empty[Arch[NT, NW]], visited) /: n.archs) {
          case ((prevArchs, prevVisited), (weight, child)) =>
            val (newChild, subTreeVisited) = mapImp(child, prevVisited)
            val updatedArches = (fWeights(weight), newChild) :: prevArchs
            updatedArches -> subTreeVisited
        }
        val newNode = new Node(fValues(n.value), newArchs.reverse)
        newNode -> (newVisited + (n -> newNode))
      }

    dg match {
      case e: Empty => e
      case node: Node[T, W] => mapImp(node, Map.empty)._1
    }

  }

  /**
   * O(n), n = number of nodes
   */
  def breadthFirstTake[T, S](g: DirectedGraph[T])(f: PartialFunction[T, S]): List[S] = {

    def traverseRec(toVisit: Queue[DirectedGraph[T]], acc: List[S]): List[S] = toVisit.headOption collect {
      case Node(value, children) =>
        val (newToVisit, toAcc) = f.lift(value) map { res =>
          (toVisit.tail ++ children, Some(res))
        } getOrElse (toVisit.tail -> None)
        traverseRec(newToVisit, toAcc map (_ :: acc) getOrElse acc)
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
        } getOrElse (toVisit.tail -> None)
        traverseRec(newToVisit, toAcc map (_ :: acc) getOrElse acc)
    } getOrElse acc.reverse

    traverseRec(g :: Nil, Nil)

  }

  /**
   * O(n)*T(f)
   */
  def breadthFirstFold[T, S](z: S, g: DirectedGraph[T])(f: PartialFunction[(S, T), S]): S = {

    def traverseRec(toVisit: Queue[DirectedGraph[T]], acc: S): S =
      toVisit.headOption collect {
        case Node(value, children) =>
          val (newToVisit, newAcc) = f.lift(acc, value) map { res =>
            (toVisit.tail ++ children, res)
          } getOrElse (toVisit.tail -> acc)
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
      visited: Set[Node[T, NoWeight]]): (AdjacencyMap, List[Node[T, NoWeight]]) =
      toVisit.headOption collect {
        case node @ Node(_, children) =>
          adjacencyMap(
            toVisit.tail ++ children.filterNot(acc contains _),
            acc + (node -> children.toSet),
            if (!visited.contains(node)) node :: breadCrumbs else breadCrumbs,
            visited + node)
        case Empty() => adjacencyMap(toVisit.tail, acc, breadCrumbs, visited)
      } getOrElse (acc, breadCrumbs reverse)

    val (node2adjacents, nodes) = adjacencyMap(Queue(g), Map.empty, Nil, Set.empty)
    val pos2node = nodes toArray

    positionalValues[Boolean](pos2node.length, pos2node.length) {
      case (i, j) => node2adjacents get (pos2node(i)) map (_.contains(pos2node(j))) getOrElse false
    }

  }

  def atSameConnectedComponent[T, W](a: Node[T, W], b: Node[T, W]): Boolean = {
    import scala.collection.immutable.Queue
    case class ExplorationState(
      lastVisited: Option[Node[T, W]],
      visited: Set[Node[T, W]],
      toVisit: Queue[Node[T, W]])

    def bidirectionalSearch(
      fromA: ExplorationState,
      fromB: ExplorationState): Boolean = {
      def explorationStep(st: ExplorationState, goal: Node[T, W]): ExplorationState =
        st.toVisit.headOption map { currentNode =>
          if (currentNode.children contains goal) st
          else {
            ExplorationState(
              Some(currentNode),
              st.visited + currentNode,
              st.toVisit.tail ++ currentNode.children.filterNot(st.visited contains _))
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
      ExplorationState(None, Set.empty, Queue(b)))

  }

  def minDistancesDijkstra[T, W](root: Node[T, W])(
    implicit
    weightNumeric: Numeric[W]): Map[Node[T, W], W] = {

    import weightNumeric.{ mkNumericOps, mkOrderingOps }

    type NodeSet = Set[Node[T, W]]
    type Distances = Map[Node[T, W], W]

    def dijkstraIteration(visited: NodeSet, distances: Distances): (NodeSet, Distances) = {
      val toVisitDistances = distances.filter(entry => !visited.contains(entry._1))
      if (toVisitDistances.nonEmpty) {
        val (selectedNode, d) = toVisitDistances.minBy(_._2)
        val newDistances = (distances /: selectedNode.archs) {
          case (prevDistances, (delta, goalNode)) =>
            val newDistanceValue = prevDistances.get(goalNode) collect {
              case prevDistance if prevDistance <= d + delta => prevDistance
            } getOrElse (d + delta)
            prevDistances.updated(goalNode, newDistanceValue)
        }
        dijkstraIteration(visited + selectedNode, newDistances)
      } else (visited, distances)

    }

    dijkstraIteration(Set.empty, Map(root -> weightNumeric.zero))._2

  }

  /**
   *
   * This function generates a reference map from each node
   * to the set of parents of that node provided the complete collection
   * of nodes in the graph.
   *
   * O(n+m), n = Number of nodes in the graph, m = number of arches
   *
   * @param nodes Full collection of nodes in the graph (target forest)
   * @return
   */
  def findParents[T, W](nodes: Seq[Node[T, W]]): Map[Node[T, W], Set[Node[T, W]]] = {

    type Node2Parent = Map[Node[T, W], Set[Node[T, W]]]

    def depthFirstNode2Parent(
      visited: Set[Node[T, W]],
      toVisit: Seq[Node[T, W]])(acc: Node2Parent): Node2Parent =
      if (toVisit.isEmpty) acc
      else {
        val (Seq(current), pending) = toVisit.splitAt(1)
        val updatedMap = (acc /: current.children) {
          (prevMap, child) =>
            prevMap + {
              (child, prevMap.getOrElse(child, Set.empty) + current)
            }
        }
        val newToVisit = current.children.filterNot(visited contains _) ++ pending
        depthFirstNode2Parent(visited + current, newToVisit)(updatedMap)
      }

    depthFirstNode2Parent(Set.empty, nodes)(Map.empty)

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
          case Element(_, next) => tailRecLength(next, acc + 1)
          case _ => acc
        }
        tailRecLength(this)
      }

    }

    class LoopElement[T](x: T)(fNext: Element[T] => LinkedList[T]) extends Element(x, Empty) {
      override val next: LinkedList[T] = fNext(this)
    }

    object LoopElement {
      def apply[T](x: T)(fNext: Element[T] => LinkedList[T]): LoopElement[T] =
        new LoopElement(x)(fNext)
    }

    case object Empty extends LinkedList[Nothing] {
      override def length: Int = 0
      override def next: LinkedList[Nothing] =
        throw new RuntimeException("Serial crash: Asking EMPTY for next")
    }

    object TyingTheNot {

      class Node[T](val value: Option[T], next: => Option[Node[T]]) {
        lazy val maybeNext: Option[Node[T]] = next
      }

      object Node {
        def apply[T](v: T, lazyNext: => Node[T]): Node[T] = new Node(Some(v), Some(lazyNext))
        def apply[T](v: T): Node[T] = new Node[T](Some(v), None)
      }

      /**
        * Detects loops in a Linked list providing their first node if found
        * O(n), n = number of elements in the list
        *
        * @param list Linked list
        * @return `Some(First node in the loop)` if found, `None` otherwise
        */
      def detectLoop[T](list: Node[T]): Option[Node[T]] = {

        def race(slow: Node[T], fast: Node[T], firstIteration: Boolean): Option[Node[T]] =
          if(!firstIteration && slow == fast) Some(slow)
          else {
            val maybeNextStep = for {
              nextSlow <- slow.maybeNext
              fastNext <- fast.maybeNext
              nextFast <- fastNext.maybeNext
            } yield (nextSlow, nextFast)
            maybeNextStep match { //Just a map, pattern match to keep tail recursion
              case Some((nextSlow, nextFast)) => race(nextSlow, nextFast, false)
              case _ => None
            }
          }

        race(list, list, true)
      }

    }

  }

}

