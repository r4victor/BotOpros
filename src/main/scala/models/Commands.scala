package botopros.models

sealed abstract class Command

case class CreatePollCommand(
  name: String,
  anonymous: Option[Boolean] = None,
  visibility: Option[PollVisibility] = None,
  start: Option[Long] = None,
  end: Option[Long] = None
) extends Command

case class ListCommand() extends Command

case class DeletePollCommand(uid: Int) extends Command

case class StartPollCommand(uid: Int) extends Command

case class StopPollCommand(uid: Int) extends Command

case class ResultCommand(uid: Int) extends Command

case class BeginCommand(uid: Int) extends Command

case class EndCommand() extends Command

case class ViewCommand() extends Command

case class AddQuestionCommand(
  question: String,
  kind: String,
  options: List[String] = List()
) extends Command

case class DeleteQuestionCommand(n: Int) extends Command

case class AnswerCommand(n: Int, answer: String) extends Command