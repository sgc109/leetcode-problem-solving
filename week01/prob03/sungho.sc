object Solution {
  def partitionLabels(S: String): List[Int] = {
    val intervals = S.zipWithIndex
      .groupBy(_._1)
      .map {
        case (_, v) => {
          val nums = v.map(_._2)
          (nums.min, nums.max)
        }
      }.toList
      .sorted

    intervals.tail.foldLeft(List(intervals.head)) {
      case (ranges: List[(Int, Int)], (left, right)) => {
        val (lastL, lastR) = ranges.head
        if (lastR < left)
          (left, right) :: ranges
        else
          (lastL, lastR.max(right)) :: ranges.tail
      }
    }.reverse
      .map {
        case (l, r) => r - l + 1
      }
  }
}
