package botopros.models

case class StateData(
  polls: Map[Int, Poll] = Map(),
  context: Map[Int, Int] = Map(),
  lastuid: Int = 0
)