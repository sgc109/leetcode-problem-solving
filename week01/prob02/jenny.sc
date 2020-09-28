import scala.annotation.tailrec

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    getResult(matrix, target, matrix.length - 1, 0) match {
      case Some(toReturn) => toReturn
      case _ => false
    }
  }

  @tailrec
  private def getResult(matrix: Array[Array[Int]], target: Int, row: Int, col: Int): Option[Boolean] = {
    if (row < 0 || col == matrix(0).length) return None

    if (matrix(row)(col) > target) getResult(matrix, target, row - 1, col)
    else if (matrix(row)(col) < target) getResult(matrix, target, row, col + 1)
    else Some(true)
  }
}

//Solution.searchMatrix(Array(Array(1, 4, 7, 11, 15), Array(2, 5, 8, 12, 19), Array(3, 6, 9, 16, 22), Array(10, 13, 14, 17, 24), Array(18, 21, 23, 26, 30)), 5)
//[[1, 4, 7, 11, 15], [2, 5, 8, 12, 19], [3, 6, 9, 16, 22], [10, 13, 14, 17, 24], [18, 21, 23, 26, 30]]
//5
