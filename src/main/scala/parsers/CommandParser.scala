package botopros.parsers

import botopros.models._

object CommandParser extends CreatePollParser with AddQuestionParser with AnswerParser {
  def apply(in: java.lang.CharSequence) = parse(command, in)

  def command: Parser[Command] = (
    poll |
    primitive("/list", ListCommand()) |
    uidCommand("/delete_poll", (uid => DeletePollCommand(uid))) |
    uidCommand("/start_poll", (uid => StartPollCommand(uid))) |
    uidCommand("/stop_poll", (uid => StopPollCommand(uid))) |
    uidCommand("/result", (uid => ResultCommand(uid))) |
    uidCommand("/begin", (uid => BeginCommand(uid))) |
    primitive("/end", EndCommand()) |
    primitive("/view", ViewCommand()) |
    addQuestion |
    uidCommand("/delete_question", (n => DeleteQuestionCommand(n))) |
    answer
  )
}