object Solution {

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    def findFirstCol(col: Int, firstRow: Array[Int]): Int = {
      if (col == firstRow.length - 1 || target < firstRow(col + 1))
        col
      else
        findFirstCol(col + 1, firstRow)
    }

    def findTarget(row: Int, col: Int): Boolean = {
      val num = matrix(row)(col)
      if (num == target) true
      else if (col > 0 && num > target)
        findTarget(row, col - 1)
      else if (row < matrix.length - 1 && num < target)
        findTarget(row + 1, col)
      else
        false
    }

    if (matrix.length == 0 || matrix(0).length == 0)
      false
    else {
      val col = findFirstCol(0, matrix(0))
      findTarget(0, col)
    }
  }
}
