import scala.annotation.tailrec

object Solution {
  def prisonAfterNDays(cells: Array[Int], N: Int) = {

    def getNext(current: Int) = (~(current << 1) ^ (current >> 1)) & 0x7e

    @tailrec
    def mkResult(history: Map[Int, Int], skip: Boolean = false, step: Int, current: Int): Int = {
      if (step == 0) return current
      if (skip) mkResult(history, skip, step - 1, getNext(current))
      else {
        if (history.contains(current)) {
          if (step % (history(current) - step) == 0) return current
          mkResult(history, !skip, step % (history(current) - step) - 1, getNext(current))
        }
        else {
          mkResult(history.+(current -> step), skip, step - 1, getNext(current))
        }
      }
    }

    var result = mkResult(Map(), false, N, Integer.parseInt(cells.mkString, 2))

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
