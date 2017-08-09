package org.pfcoperez.dailyalgorithm

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty

object Combinatorics {

  /**
   *
   * Depth first brute force template, a kind of pruned fold where the collection
   * over which it iterates is dynamically generated.
   *
   * O(n^p), n=greatest analysis result size, p=longest chain of states
   *
   * @param z              Starting state.
   * @param analysis       Function which explores what paths starts from the current state.
   * @param isSolution     Function determining whether the current state is a solution or not.
   * @param stepSynthesis  Function which generates a new state from the current and a chosen path.
   * @return               A list of all reached solutions
   *
   */
  def bruteForce[T, S](z: S)(
    analysis: S => List[T])(
    isSolution: S => Boolean)(
    stepSynthesis: (S, T) => S): List[S] = {

    @tailrec
    def bruteForceStep(toExplore: List[S], acc: List[S]): List[S] = toExplore match {
      case st :: _ =>
        val newStates = analysis(st) map (stepSynthesis(st, _))
        bruteForceStep(
          newStates ::: toExplore.tail,
          newStates.filter(isSolution) ::: acc)
      case _ => acc
    }

    bruteForceStep(z :: Nil, Nil)
  }

  /**
   * Lazy Depth first brute force template, it returns an exploration streams on-demand providing as many solutions
   * as asked.
   *
   * Generate the stream: O(1)
   * Test a case: O(1)
   *
   * Use example: Generating 100 length 4 words:
   *
   * val result = lazyBruteForce("") { st => if(st.length < 4) ('a' to 'z').toList else Nil } {
   *   st => st.length >= 4
   * } (_ + _)
   *
   * @param z              Starting state.
   * @param analysis       Function which explores what paths starts from the current state.
   * @param isSolution     Function determining whether the current state is a solution or not.
   * @param stepSynthesis  Function which generates a new state from the current and a chosen path.
   * @return               Lazily computed stream of solutions.
   */
  def lazyBruteForce[T, S](z: S)(
    analysis: S => List[T])(
    isSolution: S => Boolean)(
    stepSynthesis: (S, T) => S): Stream[S] = {

    lazy val solutionsStream: Stream[List[S]] = (z :: Nil) #:: solutionsStream flatMap {
      case st :: restToExplore => Stream((analysis(st) map (stepSynthesis(st, _))) ::: restToExplore)
      case Nil => Empty
    }

    solutionsStream collect {
      case s :: _ if isSolution(s) => s
    }

  }

  /**
   * Cartesian product of the elements of a sequence of collections
   *
   * O(N1*N2*...*NM), Ni = Each collection size
   *
   */
  def cartesianProduct[T](collections: Seq[Seq[T]]): Seq[Seq[T]] =
    (collections :\ Seq(Seq.empty[T])) {
      (collection, partialProducts) =>
        for {
          partialRes <- partialProducts
          v <- collection.reverse
        } yield v +: partialRes
    }

  import cats._
  import cats.implicits._

  /**
   * Cartesian product of the elements of a sequence of collections, using
   * Applicative "ap" operation.
   *
   * O(N1*N2*...*NM), Ni = Each collection size
   *
   */
  def cartesianProductWithCats[T](collections: List[T]*): Seq[Seq[T]] =
    (List(List.empty[T]) /: collections) { (acc, col) =>
      Applicative[List].ap(acc.map((right: List[T]) => (x: T) => x :: right))(col)
    }

  /**
   * Cartesian product of the elements of a sequence of collections, using
   * Applicative "ap" operation.
   *
   * O(N1*N2*...*NM), Ni = Each collection size
   *
   */
  def improvedCartesianProductWithCats[T](collections: List[T]*): List[List[T]] =
    collections.toList.sequence

}
