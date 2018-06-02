package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees.binary

import cats.{Later, Now}
import org.scalatest.{FlatSpec, Inside, Matchers}

class RedBlackTreeSpec extends FlatSpec with Matchers with Inside {

  import RedBlackTree._

  "RedBlackTree" should "grow preserving parenthood relations" in {
    val grownTree = ((BlackRoot(Now(BlackEmpty), -1, Now(BlackEmpty)): BinaryTree[Int]) /: List(2, 1, 0, 4, 3, 5, 6))(
      insert(_)(_)
    )

    inside(grownTree) {
      case BlackRoot(_, -1, right) =>
        inside(right.value) {
          case RedBlackNode(left2, 2, _, right2) =>
            inside(left2.value) {
              case RedBlackNode(left1, 1, _, right1) =>
                inside(left1.value) {
                  case leaf @ RedBlackNode(_, 0, _, _) =>
                    val goneAndBack = for {
                      up1 <- leaf.parent
                      up2 <- up1.asInstanceOf[RedBlackNode[Int]].parent
                      down1 <- up2.left
                      down0 <- down1.asInstanceOf[RedBlackNode[Int]].left
                    } yield down0
                    val RedBlackNode(_, zero, _, _) = goneAndBack.value
                    zero shouldBe 0
                }
            }
            inside(right2.value) {
              case RedBlackNode(left4, 4, _, right4) =>
                inside(left4.value) {
                  case three @ RedBlackNode(_, 3, _, _) =>
                    val goneAndBack = for {
                      up4 <- three.parent
                      up2 <- up4.asInstanceOf[RedBlackNode[Int]].parent
                      down4 <- up2.right
                      down3 <- down4.asInstanceOf[RedBlackNode[Int]].left
                    } yield down3
                    val RedBlackNode(_, down3, _, _) = goneAndBack.value
                    down3 shouldBe 3
                }
                inside(right4.value) {
                  case five @ RedBlackNode(_, 5, _, _) =>
                    val goneAndBack = for {
                      up4 <- five.parent
                      up2 <- up4.asInstanceOf[RedBlackNode[Int]].parent
                      down4 <- up2.right
                      down5 <- down4.asInstanceOf[RedBlackNode[Int]].right
                    } yield down5
                    val RedBlackNode(_, down5, _, _) = goneAndBack.value
                    down5 shouldBe 5
                }
            }
        }
    }
  }

  def inOrderTraverse[T](binaryTree: BinaryTree[T]): List[T] = binaryTree match {
    case RedBlackNode(left, v, _, right) =>
      inOrderTraverse(left.value) ::: v +: inOrderTraverse(right.value)
    case _ => Nil
  }

  def preOrderTraverse[T](binaryTree: BinaryTree[T]): List[T] = binaryTree match {
    case RedBlackNode(left, v, _, right) =>
      v +: preOrderTraverse(left.value) ::: preOrderTraverse(right.value)
    case _ => Nil
  }

  it should "preserve parenthood relations after rotations" in {

    val tree = ((BlackRoot(Now(BlackEmpty), -1, Now(BlackEmpty)): BinaryTree[Int]) /: List(2, 1, 0, 4, 3, 5, 6))(insert(_)(_))
    val target = tree.asInstanceOf[BlackRoot[Int]].right.value.asInstanceOf[RedBlackNode[Int]]

    val leftRotated = leftRotation(target)(Later(RedBlackNode(Later(???), -10, false, Later(???))(Later(???)))).value.asInstanceOf[RedBlackNode[Int]]
    val rightRotatedAfterLeftRotated = rightRotation(leftRotated)(Later(RedBlackNode(Later(???), -10, false, Later(???))(Later(???)))).value

    withClue("preserve binary trees condition") {
      inOrderTraverse(target) shouldBe inOrderTraverse(leftRotated)
      inOrderTraverse(leftRotated) shouldBe inOrderTraverse(rightRotatedAfterLeftRotated)
    }

    withClue("right rotation after left rotation should act as identity") {
      preOrderTraverse(target) shouldBe preOrderTraverse(rightRotatedAfterLeftRotated)
      preOrderTraverse(target) should not be preOrderTraverse(leftRotated)
    }
  }

}
