import scala.annotation.tailrec

object Solution {

  @tailrec
  def next(cache: Set[Vector[Int]], cells: Vector[Int], N: Int): Vector[Int] = {
    def transform(cells: Vector[Int]) = {
      val seq = for (index <- cells.indices)
        yield {
          if (index == 0 || index == cells.length - 1) {
            0
          } else if (cells(index - 1) == cells(index + 1)) {
            1
          } else {
            0
          }
        }
      seq.toVector
    }

    val remainDays = if (cache.contains(cells)) N % 14 else N
    if (remainDays == 0) {
      cells
    } else {
      next(cache + cells, transform(cells), remainDays - 1)
    }
  }

  def prisonAfterNDays(cells: Array[Int], N: Int): Array[Int] = {
    next(Set(), cells.toVector, N).toArray
  }
}

Solution.prisonAfterNDays(Array(0, 1, 0, 1, 1, 0, 0, 1), 7)
//[0, 0, 1, 1, 0, 0, 0, 0]
Solution.prisonAfterNDays(Array(0, 1, 1, 1, 0, 0, 0, 0), 99)
//[0, 0, 1, 0, 0, 1, 1, 0]
Solution.prisonAfterNDays(Array(1, 0, 0, 1, 0, 0, 1, 0), 1000000000)
//[0, 0, 1, 1, 1, 1, 1, 0]