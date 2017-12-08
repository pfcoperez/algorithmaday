package org.pfcoperez.dailyalgorithm.geometry

import org.scalatest.{ Matchers, WordSpec }

import org.pfcoperez.dailyalgorithm.Geometry._

class MinkowskiSpec extends WordSpec with Matchers {

  implicit def tuple2point(p: (Double, Double)): Point = Vect(p._1, p._2)

  "Convex Minkowski additions" when {

    "Used to add convex polygons" should {

      val square: Set[Point] = Set(
        (0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0))

      val triangle: Set[Point] = Set(
        (0.0, 0.0), (0.5, 0.5), (1.0, 0.0))

      val octagon: Set[Point] = Set(
        (0.0, 0.25), (0.175, 0.175), (0.25, 0.0), (0.175, -0.175),
        (0.0, -0.25), (-0.175, -0.175), (-0.25, 0.0), (-0.175, 0.175))

      "provide thr right result for squares plus triangles" in {

        val result = convexMinkowskiAddition(square, triangle)

        result.get.take(7) should contain theSameElementsInOrderAs List[Vect](
          (0.0, 0.0),
          (0.0, 1.0),
          (0.5, 1.5),
          (1.5, 1.5),
          (2.0, 1.0),
          (2.0, 0.0),
          (1.0, 0.0))

      }

      /*"provide thr right result for trinagles plus octagons" in {

        val result = convexMinkowskiAddition(triangle, octagon)

        result.get foreach println

      }*/

    }

  }

}
