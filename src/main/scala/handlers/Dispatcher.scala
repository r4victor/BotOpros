package botopros.handlers

import botopros.helpers.State
import botopros.handlers.Handlers._
import botopros.parsers._
import botopros.models._

object Dispatcher {
  def handle(text: String, user: User): BotState = CommandParser(text) match {
    case CommandParser.Success(c,_) => c match {
      case c:CreatePollCommand => createPoll(c, user.id)
      case c:ListCommand => list
      case c:DeletePollCommand => deletePoll(c, user.id)
      case c:StartPollCommand => startPoll(c, user.id)
      case c:StopPollCommand => stopPoll(c, user.id)
      case c:ResultCommand => result(c)
      case c:BeginCommand => begin(c, user.id)
      case c:EndCommand => end(user.id)
      case c:ViewCommand => view(user.id)
      case c:AddQuestionCommand => addQuestion(c, user.id)
      case c:DeleteQuestionCommand => deleteQuestion(c, user.id)
      case c:AnswerCommand => answer(c, user)
    }
    case _ => State.unit(Messages.unknownCommand)
  }
}