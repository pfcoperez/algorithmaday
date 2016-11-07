package org.pfcoperez.dailyalgorithm.geometry

import org.pfcoperez.dailyalgorithm.Geometry.Vect
import org.pfcoperez.dailyalgorithm.Geometry.Primitives._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class SimplexVolumeSpec extends FlatSpec with Matchers {

  "simplexVolume" should "provide right results for triangle areas" in {

    val triangle: Seq[Product] = Seq(
      Vect(0,0),
      Vect(0,2),
      Vect(1,0)
    )

    simplexVolume(triangle).map(math.abs(_)) shouldBe Success(1.0)

  }

}
