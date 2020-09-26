object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = 
    if (matrix.length == 0 || matrix(0).length == 0)
      false
    else {
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
      findTarget(0, matrix(0).length - 1)
    }
}
