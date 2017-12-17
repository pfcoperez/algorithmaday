package org.pfcoperez.dailyalgorithm

import org.pfcoperez.dailyalgorithm.Algebra.Matrix
import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix
import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix.Implicits._

import scala.util.{ Success, Try }

object Geometry {

  case class Vect(x: Double, y: Double) {
    def +(that: Vect): Vect = Vect(this.x + that.x, this.y + that.y)
    def -(that: Vect): Vect = Vect(this.x - that.x, this.y - that.y)
    def dot(that: Vect): Double = this.x * that.x + this.y * that.y
    def mod: Double = Math.sqrt(x * x + y * y)
  }

  type Point = Vect
  type NPoint = Product

  /* Angle formed by CA & CB segments: O(1) */
  def alpha(A: Point, C: Point, B: Point) = {
    val (a, b) = (A - C, B - C)
    val ret = Math.acos((a dot b) / (a.mod * b.mod))
    if (ret.isInfinite || ret.isNaN) Double.NegativeInfinity else ret
  }

  /* Angle of formed by pq & the x-axis */
  def angle(p: Point, q: Point): Double =
    if (q.y == p.y && q.x >= p.x) 0.0
    else if (q.y == p.y) math.Pi
    else if (q.x == p.x && q.y >= p.y) math.Pi / 2.0
    else if (q.x == p.x) 3.0 * math.Pi / 2.0
    else math.atan((q.y - p.y) / (q.x - p.y))

  /*
    Gift wrapping Convex Hull algorithm.
    O(nm) where: n = Number of points in input & m = number of points in Convex Hull Polygon
   */
  def giftWrappingConvexHull(points: Set[Point]): Option[List[Point]] =
    if (points.size < 3) None
    else {
      val zeroth = Vect(0, -1) //Not part of the convex hull, to be removed

      val first = points.min(Primitives.fromLeftToRightAndTopToBottom) //Left-upper most point

      def recCH(ch: List[Point], remaining: Set[Point]): List[Point] =
        ch match {
          case current :: prev :: _ if !remaining.isEmpty =>
            //Select `next` to maximize angle between current2prev and current2next segments
            val next = remaining.maxBy(alpha(prev, current, _))
            if (next == first) ch else recCH(next :: ch, remaining - next)
          case _ => ch
        }

      Some(recCH(first :: zeroth :: Nil, points) init)
    }

  /*
    Gift wrapping Convex Hull algorithm.
    O(nm) where: n = Number of points in input & m = number of points in Convex Hull Polygon
   */
  def fasterGiftWrappingConvexHull(points: Set[Point]): Option[List[Point]] =
    if (points.size < 3) None
    else {

      import Primitives.{ pointRelative2boundary, Above, fromLeftToRightAndTopToBottom }

      val first = points.min(fromLeftToRightAndTopToBottom)

      def recCH(ch: List[Point], remaining: Set[Point]): List[Point] =
        ch match {
          case prev :: _ if !remaining.isEmpty =>
            val next = (remaining.head /: remaining.tail) {
              (currentCandidate, newCandidate) =>
                val currentSegment = Seq(prev, currentCandidate)
                if (pointRelative2boundary(newCandidate, currentSegment) == Success(Above))
                  newCandidate
                else
                  currentCandidate
            }
            if (ch.head == first ||
              pointRelative2boundary(next, Seq(ch.head, first)) == Success(Above)) recCH(next :: ch, remaining - next) else ch
          case _ => ch
        }

      Some(recCH(first :: Nil, points - first).reverse)
    }

  /**
   * Minkowski addition for convex polygons.
   * O(n+m), n = number of vertices in polygon `a`, m = number of vertices in polygon 'b'
   *
   * NOTE: The Minkowski addition time complexity is O(m+m), however, this
   *       implementation makes sure its inputs are convex polygons, therefore, as long
   *       as these inputs are actually convex hulls, the actual time complexity is O(n^2 + m^2).
   *       If the initial check is removed, the time complexity will automatically become O(n+m)
   */
  def convexMinkowskiAddition(a: Set[Point], b: Set[Point]): Option[List[Point]] =
    for {
      convexPolygonA <- fasterGiftWrappingConvexHull(a)
      convexPolygonB <- fasterGiftWrappingConvexHull(b)
    } yield {

      import Primitives.{ pointRelative2boundary, Above, fromLeftToRightAndTopToBottom }

      def mergePoint(aIncrements: Stream[Point], bIncrements: Stream[Point], acc: List[Point]): List[Point] =
        if (Seq(aIncrements, bIncrements).exists(_.isEmpty)) acc
        else {
          val current = acc.head

          val Seq(aCandidate, bCandidate) = Seq(aIncrements, bIncrements) map { increments =>
            current + increments.head
          }

          if (pointRelative2boundary(aCandidate, Seq(current, bCandidate)).map(_ == Above).getOrElse(false)) {
            mergePoint(aIncrements.tail, bIncrements, aCandidate :: acc)
          } else {
            mergePoint(aIncrements, bIncrements.tail, bCandidate :: acc)
          }

        }

      val polygons = Seq(convexPolygonA, convexPolygonB)
      val maxLength = polygons.map(_.size).max
      val startPoint = (a ++ b).min(fromLeftToRightAndTopToBottom)

      val Seq(aIncrements, bIncrements) = polygons map { polygon =>
        val asStream = if (polygon.size < maxLength) {
          lazy val repeated: Stream[Point] = polygon.toStream #::: repeated
          repeated
        } else polygon.toStream #::: polygon.toStream.take(2)

        (asStream zip asStream.tail) map {
          case (from, to) => to - from
        }

      }

      mergePoint(aIncrements, bIncrements, startPoint :: Nil).reverse
    }

  object Primitives {

    /**
     * Compute the volume of a n-dimensional simplex
     * O(n^3)
     *
     * @param simplexPoints
     * @return the length of a segment in 1D, the area of a triangle in 2D,
     *         the volume of a tetrahedron in 3D, ...
     */
    def simplexVolume(simplexPoints: Seq[NPoint]): Try[Double] = Try {

      def fact(x: Int, acc: Int = 1): Int = x match {
        case 0 => acc
        case _ => fact(x - 1, x * acc)
      }

      require(
        simplexPoints.nonEmpty && simplexPoints.map(_.productArity).toSet.size == 1,
        "All points should share dimension")
      val d = simplexPoints.head.productArity
      require(
        simplexPoints.size == d + 1,
        "The simplex should consist of as many points as its dimension + 1")

      val M: Matrix[Double] = simplexPoints.map { point =>
        ((point.productIterator map { case v: Double => v }) ++ Iterator(1.0)) toIndexedSeq
      } toIndexedSeq

      M.det / fact(d)

    }

    trait RelativePosition
    object Above extends RelativePosition
    object Below extends RelativePosition
    object On extends RelativePosition

    /**
     * Provides the relative position of a point to a n-dimensional boundary:
     *   a point, a line, a plane, ...
     *
     * @param p
     * @param boundary n points determining the boundary
     * @return Relative position of `p` to `boundary`:
     *         Above (left or above) \/ On \/ Below (right or below)
     */
    def pointRelative2boundary(p: NPoint, boundary: Seq[NPoint]): Try[RelativePosition] =
      simplexVolume(boundary :+ p) map {
        case v if v > 0 => Above
        case v if v < 0 => Below
        case 0 => On
      }

    val fromLeftToRightAndTopToBottom = new Ordering[Vect] {
      def compare(va: Vect, vb: Vect): Int =
        Seq(va, vb) map {
          v: Vect => if (va.x != vb.x) v.x else v.y
        } reduce (_ compare _) toInt
    }

  }

}
