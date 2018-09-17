package botopros.models

sealed abstract class PollVisibility

case class Afterstop() extends PollVisibility

case class Continuous() extends PollVisibility