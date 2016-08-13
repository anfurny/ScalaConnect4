import scala.reflect.ClassTag
import scala.util.matching.Regex

/**
  * Created by Alex on 8/13/16.
  */

object Connect4 {
  val empty = "."

  def main(args: Array[String]) {
    var board = Array.ofDim[String](5,5)
    clearBoard(board)
    printBoard(board)
    var activePlayer = 1
    do {
      solicitAndApplyMove(activePlayer, board)
      printBoard(board)
      activePlayer = activePlayer % 2 + 1
    } while (checkWinner(board) == 0)
    println ("Winner is: " + checkWinner(board).toString)
  }

  def getSymbol(activePlayer: Integer): Char = {
    if (activePlayer == 1) return 'X'
    'O'
  }

  /**
    * Force a player to enter a valid move and update the board object
    * @param activePlayer
    * @param board
    * @return
    */
  def solicitAndApplyMove(activePlayer: Integer, board: Array[Array[String]]): Array[Array[String]] = {
    var retry = false
    do {
      retry = false
      println ("Enter Column Player #" + activePlayer)
      try {
        val column = scala.io.StdIn.readLine().toInt
        if (column < 0 || column > 4) {
          throw new Exception("bullshit move")
        }
        placePiece(board, column, getSymbol(activePlayer))
      } catch {
        case _: Throwable => {
          println("No good. Retry.")
          retry = true
        }
      }
    } while (retry)
    board
  }

  /**
    * Checks if a given array of spaces has 4 in a row of the same type
    * @param row
    * @return
    */
  def hasConsecutive4(row: Array[String]): String = {
    val string = row.reduce(_ + _)
    val pattern = new Regex(".?(.)\\1\\1\\1.?")
    val result = pattern.findFirstIn(string)
    if (result.isEmpty) empty else "" + result.getOrElse(empty).charAt(1)
  }

  /**
    * Determines the current winner on the board
    *  -1 = the game is over and drawn
    *  0  = the game is still going
    *  1  = winner player 1
    *  2  = winner player 2
    * @param board
    * @return
    */
  def checkWinner(board: Array[Array[String]]): Integer = {
    val rotations = Array(0, 45, 90, 135)
    def getWinner(board: Array[Array[String]]): Integer = {
      val winner = board.map(hasConsecutive4).filter({_ != empty})
      var winnerNum = winner.map(str => if (str=="X") 1 else 2)
      if (winnerNum.length == 0) 0 else winnerNum(0)
    }
    val winners = rotations.map(degrees => getWinner(getRotatedMatrix(board, degrees))).filter(_ > 0)
    if (winners.length > 0)
      return winners(0)
    if (board.map(_.reduce(_ + _)).reduce(_ + _).indexOf(empty) == -1)
      return -1
    return 0
  }

  /**
    * Returns a new matrix rotated by degrees (counter-clockwise), use negative to go clockwise
    * @param board
    * @param degrees
    * @tparam T
    * @return
    */
  def getRotatedMatrix[T: ClassTag](board: Array[Array[T]], degrees: Double): Array[Array[T]] = {
    def round(d: Double): Double = { (d * 10000000000.0).round / (10000000000.0)};
    val radians = degrees * Math.PI / 180

    //treat as x,y (since it's indistinguishable from y,x) for rotation purposes
    val indexedBoard = for (x <- board.indices; y <- board(x).indices) yield (board(x)(y), x, y)
    val rotatedBoard = indexedBoard.map({tuple =>
      val value = tuple._1
      val x = tuple._2
      val y = tuple._3
      val newY = round(x*Math.sin(radians) + y*Math.cos(radians))
      val newX = round(x*Math.cos(radians) - y*Math.sin(radians))
      (value, newX, newY)
    })

    val solution = rotatedBoard.groupBy(_._2).toList.sortWith(_._1 < _._1)
    val solution2 = solution.map(row => row._2.sortWith(_._3 < _._3).map(_._1).toArray[T]).toArray[Array[T]]
    solution2
  }

  /**
    * Updates board object in-place by dropping a piece in
    * @param board
    * @param column
    * @param value
    */
  def placePiece(board: Array[Array[String]], column: Integer, value: Char): Unit = {
    val y = 0
    for (y <- board.length - 1 to 0 by -1) {
      if (board(y)(column).equals(empty)) {
        board(y)(column) = "" + value
        return
      }
    }
    throw new Exception("No empty space in that column")
  }

  def printBoard(board: Array[Array[String]]): Unit = {
    def makeRow(row: Array[String]) = { row.foldLeft("") { _ + _} };
    var output = board.foldLeft("") { _ + "\n" + makeRow(_)}
    print("The board: ")
    print(output)
  }

  def clearBoard(board: Array[Array[String]]): Unit = {
    val x, y = 0
    for (x <- board.indices;
         y <- board(x).indices) {
      board(y)(x) = empty
    }
  }
}