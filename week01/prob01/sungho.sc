object Solution {
  private def changeState(state: Vector[Int]) = {
    val nextState = for (i <- state.indices)
      yield
        if (i > 0 &&
          i < state.length - 1 &&
          state(i - 1) == state(i + 1))
          1
        else
          0
    nextState.toVector
  }

  private def solve(memo: Map[Vector[Int], Int], cells: Vector[Int], restDays: Int): Vector[Int] = {
    val reducedDays =
      if (memo.contains(cells))
        restDays % (memo(cells) - restDays)
      else
        restDays

    if (reducedDays == 0)
      cells
    else
      solve(
        memo + (cells -> restDays),
        changeState(cells),
        reducedDays - 1
      )
  }

  def prisonAfterNDays(cells: Array[Int], N: Int): Array[Int] =
    solve(
      Map(),
      cells.toVector,
      N
    ).toArray
}
