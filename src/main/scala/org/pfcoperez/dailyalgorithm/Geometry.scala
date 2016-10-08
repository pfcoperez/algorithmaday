package org.pfcoperez.dailyalgorithm

object Geometry {

  case class Vect(x: Double, y: Double) {
    def -(that: Vect): Vect = Vect(this.x-that.x, this.y-that.y)
    def dot(that: Vect): Double = this.x*that.x + this.y*that.y
    def mod: Double = Math.sqrt(x*x+y*y)
  }

  type Point = Vect

  /* Angle formed by CA & CB segments: O(1) */
  def alpha(A: Point, C: Point, B: Point) = {
    val (a, b) = (A - C, B - C)
    val ret = Math.acos((a dot b)/(a.mod * b.mod))
    if(ret.isInfinite || ret.isNaN) Double.NegativeInfinity else ret
  }

  /*
    Gift wrapping Convex Hull algorithm.
    O(nm) where: n = Number of points in input & m = number of points in Convex Hull Polygon
   */
  def giftWrappingConvexHull(points: Set[Point]): Option[List[Point]] =
    if(points.size < 3) None
    else {
      val zeroth = Vect(0,-1) //Not part of the convex hull, to be removed
      val first = points.minBy(_.x) //Leftmost point

      def recCH(ch: List[Point], remaining: Set[Point]): List[Point] =
        ch match {
          case current::prev::_ if !remaining.isEmpty =>
            //Select `next` to maximize angle between current2prev and current2next segments
            val next = remaining.maxBy(alpha(prev, current, _))
            if(next == first) ch else recCH(next :: ch, remaining-next)
          case _ => ch
        }

      Some(recCH(first:: zeroth :: Nil, points) init)
    }

}
