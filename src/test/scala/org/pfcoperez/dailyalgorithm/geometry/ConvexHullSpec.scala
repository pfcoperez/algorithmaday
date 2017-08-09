package org.pfcoperez.dailyalgorithm.geometry

import org.scalatest.{ Matchers, WordSpec }

import org.pfcoperez.dailyalgorithm.Geometry._

class ConvexHullSpec extends WordSpec with Matchers {

  implicit def tuple2point(p: (Double, Double)): Point = Vect(p._1, p._2)

  "Gift Wrapping 2D Convex Hull algorithm" when {

    val valuesAndExpectations = Seq(
      Set[Vect]((-1.0, -1.0), (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0), (0.0, 0.0)) -> Some(Set[Vect]((1.0, -1.0), (1.0, 1.0), (-1.0, 1.0), (-1.0, -1.0))),
      Set[Vect]((-1.0, 1.0), (0.0, 0.0)) -> None,
      Set[Vect]((-1.0, 0.0), (0.0, 0.0), (1.0, 1.0), (0.5, 0.5)) -> Some(Set[Vect]((0.0, 0.0), (1.0, 1.0), (-1.0, 0.0))),
      Set[Vect]((10.0, 10.0), (20.0, 25.0), (30.0, 30.0), (10.0, 30.0)) -> Some(Set[Vect]((10.0, 30.0), (30.0, 30.0), (10.0, 10.0))),
      Set[Vect](
        (4.4, 14.0),
        (6.7, 15.25),
        (6.9, 12.8),
        (2.1, 11.1),
        (9.5, 14.9),
        (13.2, 11.9),
        (10.3, 12.3),
        (6.8, 9.5),
        (3.3, 7.7),
        (0.6, 5.1),
        (5.3, 2.4),
        (8.45, 4.7),
        (11.5, 9.6),
        (13.8, 7.3),
        (12.9, 3.1),
        (11.0, 1.1)) -> Some(Set[Vect](
          (5.3, 2.4),
          (11.0, 1.1),
          (12.9, 3.1),
          (13.8, 7.3),
          (13.2, 11.9),
          (9.5, 14.9),
          (6.7, 15.25),
          (4.4, 14.0),
          (2.1, 11.1),
          (0.6, 5.1))))

    valuesAndExpectations foreach {
      case (input, expected) => s"CH($input)" should {
        s"result in $expected" in {
          giftWrappingConvexHull(input).map(_.toSet) shouldBe expected
          fasterGiftWrappingConvexHull(input).map(_.toSet) shouldBe expected
        }
      }
    }

  }

}
