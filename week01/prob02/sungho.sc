object Solution {
  private def findFirstCol(col: Int, firstRow: Array[Int], target: Int): Int = {
    if (col == firstRow.length - 1 || target < firstRow(col + 1))
      col
    else
      findFirstCol(col + 1, firstRow, target)
  }

  private def findTarget(row: Int, col: Int, matrix: Array[Array[Int]], target: Int): Boolean = {
    val num = matrix(row)(col)
    if (num == target) true
    else if (col > 0 && num > target)
      findTarget(row, col - 1, matrix, target)
    else if (row < matrix.length - 1 && num < target)
      findTarget(row + 1, col, matrix, target)
    else
      false
  }

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    if (matrix.length == 0 || matrix(0).length == 0)
      false
    else {
      val col = findFirstCol(0, matrix(0), target)
      findTarget(0, col, matrix, target)
    }
  }
}
