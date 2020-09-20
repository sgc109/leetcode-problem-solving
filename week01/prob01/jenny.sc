import scala.annotation.tailrec
import scala.collection.mutable

object Solution {
  def prisonAfterNDays(cells: Array[Int], N: Int) = {
    val history = mutable.Map[Int, Int]()

    def getNext(current: Int) = (~(current << 1) ^ (current >> 1)) & 0x7e

    @tailrec
    def mkResult(skip: Boolean = false, step: Int, current: Int): Int = {
      if (step == 0) return current
      if (skip) mkResult(skip, step - 1, getNext(current))
      else {
        if (history.contains(current)) {
          if (step % (history(current) - step) == 0) return current
          mkResult(!skip, step % (history(current) - step) - 1, getNext(current))
        }
        else {
          history += (current -> step)
          mkResult(skip, step - 1, getNext(current))
        }
      }
    }

    var result = mkResult(false, N, Integer.parseInt(cells.mkString, 2))

    cells.indices.flatMap(_ => {
      val i1 = result & 0x1
      result >>= 1
      Seq(i1)
    }).reverse.toArray
  }
}

//Solution.prisonAfterNDays(Array(1,0,0,0,1,0,0,1), 99)
//[1,0,0,0,1,0,0,1]
//99
