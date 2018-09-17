package botopros.models

sealed abstract class Answer

case class OpenAnswer(text: String) extends Answer

case class ChoiceAnswer(choice: Int) extends Answer

case class MultiAnswer(choices: Set[Int]) extends Answer 