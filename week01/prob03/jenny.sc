import scala.annotation.tailrec

object Solution {
  def partitionLabels(S: String): List[Int] = getResult(S, List())

  @tailrec
  def getResult(str: String, result: List[Int]): List[Int] = {
    if (str.isEmpty) return result

    val idx = str.lastIndexOf(str(0))

    val i = getBunch(str, idx, str.substring(0, idx).toSet, Set())

    getResult(str.substring(i), result :+ i)
  }

  @tailrec
  def getBunch(S: String, idx: Int, set: Set[Char], history: Set[Char]): Int = {
    if (set.size == 0) return idx + 1

    val lastIdx = S.lastIndexOf(set.head)
    val newHistory = history.+(set.head)

    if (lastIdx > idx) {
      val newSet = S.substring(0, lastIdx).toSet

      if (newSet.size > set.size) getBunch(S, lastIdx, newSet.diff(history), newHistory)
      else getBunch(S, lastIdx, set.drop(1), newHistory)
    }
    else getBunch(S, idx, set.drop(1), newHistory)
  }
}

//Solution.partitionLabels("qiejxqfnqceocmy")
//"ababcbaca defegde hijhklij"
//[9,7,8]

//"qiejxqfnqceoc m y"
//[13,1,1]
