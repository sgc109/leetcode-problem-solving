import scala.annotation.tailrec

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    getResult(0, matrix, target)
  }

  @tailrec
  private def getResult(idx: Int, matrix: Array[Array[Int]], target: Int): Boolean = {
    if (idx == matrix.length) return false

    matrix(idx).contains(target) match {
      case true => true
      case _ => getResult(idx + 1, matrix, target)
    }
  }
}

//Solution.searchMatrix(Array(Array(1, 4, 7, 11, 15), Array(2, 5, 8, 12, 19), Array(3, 6, 9, 16, 22), Array(10, 13, 14, 17, 24), Array(18, 21, 23, 26, 30)), 5)
//[[1, 4, 7, 11, 15], [2, 5, 8, 12, 19], [3, 6, 9, 16, 22], [10, 13, 14, 17, 24], [18, 21, 23, 26, 30]]
//5
