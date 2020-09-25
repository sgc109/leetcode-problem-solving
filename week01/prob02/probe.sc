import scala.annotation.tailrec

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    @tailrec
    def search(x: Int, y: Int): Boolean = {
      val value = matrix(x)(y)
      if (value == target) {
        true
      } else if (y > 0 && value > target) {
        search(0, y - 1)
      } else if (x < matrix.length - 1 && value < target) {
        search(x + 1, y)
      } else {
        false
      }
    }

    if (matrix.length == 0 || matrix(0).length == 0) {
      false
    } else {
      search(0, matrix(0).length - 1)
    }
  }
}

//[
//[1,   4,  7, 11, 15],
//[2,   5,  8, 12, 19],
//[3,   6,  9, 16, 22],
//[10, 13, 14, 17, 24],
//[18, 21, 23, 26, 30]
//]

val matrix = Array(
  Array(1, 4, 7, 11, 15),
  Array(2, 5, 8, 12, 19),
  Array(3, 6, 9, 16, 22),
  Array(10, 13, 14, 17, 24),
  Array(18, 21, 23, 26, 30),
)
Solution.searchMatrix(matrix, 5)