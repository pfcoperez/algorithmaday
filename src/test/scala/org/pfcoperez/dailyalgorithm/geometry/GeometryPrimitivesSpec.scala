package org.pfcoperez.dailyalgorithm.geometry

import org.pfcoperez.dailyalgorithm.Geometry.Vect
import org.pfcoperez.dailyalgorithm.Geometry.Primitives._
import org.scalatest.{ FlatSpec, Matchers }

import scala.util.Success

class GeometryPrimitivesSpec extends FlatSpec with Matchers {

  "simplexVolume" should "provide right results for triangle areas" in {

    val triangle: Seq[Product] = Seq(
      Vect(0, 0),
      Vect(0, 2),
      Vect(1, 0))

    simplexVolume(triangle).map(math.abs(_)) shouldBe Success(1.0)

  }

  it should "provide an algorithm for the 2D above-below-on test" in {

    val segment = Seq(Vect(0, 0), Vect(2, 2))

    val abovePoint = Vect(1, 2)
    val onPoint = Vect(1, 1)
    val belowPoint = Vect(1, 0)

    pointRelative2boundary(abovePoint, segment).get shouldBe Above
    pointRelative2boundary(onPoint, segment).get shouldBe On
    pointRelative2boundary(belowPoint, segment).get shouldBe Below

  }

}
