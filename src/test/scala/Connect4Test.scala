import org.scalatest._

class Connect4Test extends FlatSpec with Matchers {
  "random experiment" should "Whatever" in {
    val x = List(Some(3), None)
      x.flatMap(x=>x)
  }

  "RotateMatrix" should "work" in {
    val matrix1 = Array(Array("A","B"), Array("C", "D"))
    val rotated = Connect4.getRotatedMatrix(matrix1, 45)
    rotated(0)(0) shouldEqual "B"
    rotated(1)(0) shouldEqual "A"
    rotated(1)(1) shouldEqual "D"
    rotated(2)(0) shouldEqual "C"

    val matrix2 = Array(Array(1,2,3))
    val rotated2 = Connect4.getRotatedMatrix(matrix2, 180)
    rotated2(0)(0) shouldEqual(3)
    rotated2(0)(1) shouldEqual(2)
  }

  "Has 4 in a row" should "work" in {
    Connect4.hasConsecutive4(Array("a","b","c","d","e")) shouldEqual "."
    Connect4.hasConsecutive4(Array("a","a","a","a","e")) shouldEqual "a"
    Connect4.hasConsecutive4(Array("a","a","a","a","a")) shouldEqual "a"
    Connect4.hasConsecutive4(Array("x","b","b","b","b")) shouldEqual "b"
    Connect4.hasConsecutive4(Array("a",".","a","a","a")) shouldEqual "."
  }

  "Check Winner" should "Work" in {
    val Board = Array(
      Array(".",".",".",".","."),
      Array(".",".",".",".","."),
      Array(".",".",".",".","."),
      Array(".",".",".",".","."),
      Array(".",".",".",".",".")
    )

    Connect4.checkWinner(Board) shouldEqual 0

    val Board2 = Array(
      Array(".",".",".",".","."),
      Array(".",".",".",".","."),
      Array(".",".",".",".","."),
      Array(".",".",".",".","."),
      Array("X","X","X","X",".")
    )

    Connect4.checkWinner(Board2) shouldEqual 1

    val Board3 = Array(
      Array(".","O",".",".","."),
      Array(".","O",".",".","."),
      Array(".","O",".",".","."),
      Array(".","O",".",".","."),
      Array(".",".",".",".",".")
    )

    Connect4.checkWinner(Board3) shouldEqual 2

    val Board4 = Array(
      Array(".",".",".",".","O"),
      Array(".",".",".","O","."),
      Array(".",".","O",".","."),
      Array(".","O",".",".","."),
      Array(".",".",".",".",".")
    )

    Connect4.checkWinner(Board4) shouldEqual 2

    val Board5 = Array(
      Array(".",".",".",".","."),
      Array(".","X",".",".","."),
      Array(".",".","X",".","."),
      Array(".",".",".","X","."),
      Array(".",".",".",".","X")
    )

    Connect4.checkWinner(Board5) shouldEqual 1

    val Board6 = Array(
      Array("X","X","X","O","X"),
      Array("O","X","X","X","O"),
      Array("X","X","O","O","X"),
      Array("X","O","X","O","X"),
      Array("X","O","X","X","X")
    )

    Connect4.checkWinner(Board6) shouldEqual -1 //draw
  }




}