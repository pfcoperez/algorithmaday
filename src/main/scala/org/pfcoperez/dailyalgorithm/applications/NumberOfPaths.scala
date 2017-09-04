package org.pfcoperez.dailyalgorithm.applications

import org.pfcoperez.dailyalgorithm.Algebra.Matrix
import org.pfcoperez.dailyalgorithm.Algebra.Matrix._
import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix.MultiplicationMethod
import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix.MultiplicationMethods._

import org.pfcoperez.dailyalgorithm.datastructures.graphs.directed._

object NumberOfPaths {

  /**
   * Number of possible paths, of length n, from `from` to `to` in the graph 'g'
   * O(m^2 * log m * n), n = number of steps, m = number of nodes in g
   *
   * PRE: For all 'v' in 'g' /\ 'w' in g, v != w
   *      (There are no to nodes with the same value)
   *
   */
  def numberOfPaths[T](g: DirectedGraph[T])(from: T, to: T, steps: Int): Int = {

    // Number of paths from any node to any other in one step (1 if they are connected)
    val oneStep = fmap(g: Matrix[Boolean])(if (_) 1 else 0)

    // Generates a list where each position contains the value of its node in de adjacency matrix
    val pos2node = (breadthFirstFold((Set.empty[T], List.empty[T]), g) {
      case (prev @ (visited, _), v) if (visited contains v) => prev
      case ((visited, breadCrumbs), v) => (visited + v, v :: breadCrumbs)
    })._2 reverse

    // Generates a map from each value in g to its position in the adjacency matrix
    val value2position: Map[T, Int] = pos2node.zipWithIndex toMap

    // Check precondition
    require(
      pos2node.size == value2position.size,
      "PRE-CONDITION VIOLATED: There are different nodes with the same value")

    implicit val _: MultiplicationMethod = new DivideAndConquer(NaiveMultiplicationMethod)

    /*
     * adjacencyMatrix^(number of steps) produces the matrix of number of paths between
     * each pair of nodes
     */
    val pathsCountMatrix = (oneStep /: (1 until steps))((prev, _) => prev * oneStep)

    // The value to index map is used to address the specified path
    pathsCountMatrix(value2position(from))(value2position(to))

  }

}
