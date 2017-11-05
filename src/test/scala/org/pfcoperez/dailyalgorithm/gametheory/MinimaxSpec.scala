package org.pfcoperez.dailyalgorithm.gametheory

import org.scalatest.{ WordSpec, Matchers }

import scala.language.implicitConversions
import scala.language.postfixOps

class MinimaxSpec extends WordSpec with Matchers {

  "Minimax#bestMovement template" can {

    "model tic-tac-toe game" should {

      trait BoardValue
      trait Player extends BoardValue
      object X extends Player
      object O extends Player
      object E extends BoardValue

      type Board = Vector[Vector[BoardValue]]
      type Score = Int
      type Position = (Int, Int)
      case class Movement(position: Position, byMaxPlayer: Boolean)

      val boardLength = 3

      val emptyBoard = Vector.fill(boardLength)(Vector.fill(boardLength)(E))

      def isMaxPlayer(player: Player): Boolean = player == X
      def whichPlayer(isMaxPlayer: Boolean): Player = if (isMaxPlayer) X else O

      implicit def validMovements(board: Board, isMaxPlayer: Boolean): Seq[Movement] =
        for {
          i <- 0 until boardLength
          j <- 0 until boardLength
          if board(i)(j) == E
        } yield Movement((i, j), isMaxPlayer)

      implicit def applyMovement(board: Board, movement: Movement): Board = {
        val Movement((i, j), isMaxPlayer) = movement
        val row = board(i)
        board.updated(i, row.updated(j, whichPlayer(isMaxPlayer)))
      }

      implicit def score(board: Board): Score = {
        for {
          player <- Seq(X, O)
          orientedBoard <- Seq(board, board.transpose)
          crossWin = orientedBoard.exists(_ == Vector.fill(boardLength)(player))
          xWin = Seq((i: Int) => boardLength - 1 - i, (i: Int) => i) exists { trans =>
            (0 until boardLength) forall { k =>
              board(k)(trans(k)) == player
            }
          }
          if (crossWin || xWin)
        } yield {
          if (isMaxPlayer(player)) 10 else -10
        }
      }.headOption getOrElse {
        val h = boardLength / 2
        if (board(h)(h) == whichPlayer(true)) 1
        else if (board(h)(h) == whichPlayer(false)) -1
        else 0
      }

      def computeBestPossibleMovement(board: Board, player: Player, maxDepth: Int = 100): Option[(Movement, Score)] =
        Minimax.bestMovement[Movement, Board, Score](board, isMaxPlayer(player), maxDepth)

      "able to detect that it can win in a lose-lose situation" in {

        val lostHand = Vector(
          Vector(O, X, E),
          Vector(O, O, E),
          Vector(X, X, O))

        computeBestPossibleMovement(lostHand, X).get._2 shouldBe -10

      }

      "able to find the best movement" in {
        computeBestPossibleMovement(emptyBoard, X).get._2 shouldBe 10
      }

      "able to tie a game a player can't win" in {

        val losingHand = Vector(
          Vector(X, O, E),
          Vector(O, O, E),
          Vector(X, X, O))

        val res = computeBestPossibleMovement(losingHand, X)
        println(res)
        res.get._2 shouldBe -1

      }

    }

  }

}
