package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

import org.scalatest.{Matchers, WordSpec}
import LinkedStructures.TyingTheNot.{detectLoop, Node => TNode}

class TyingTheNotSpec extends WordSpec with Matchers {

  "TyingTheNot linked list implementation" when {

    "presenting loops" should {

      "expose them through `detectLoop`" in {

        val linkedWithLoop = {
          lazy val header = TNode(1, TNode(2, tail))
          lazy val tail: TNode[Int] = TNode(3, header)
          header
        }

        detectLoop(linkedWithLoop) shouldBe Some(linkedWithLoop)

      }

      "not detect false loops" in {

        val linkedList = TNode(1, TNode(2, TNode(3, TNode(4))))

        detectLoop(linkedList) shouldBe None
      }

    }

  }

}
