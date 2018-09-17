package botopros.handlers

import botopros.helpers._
import botopros.models._
import botopros.parsers.CommandParser

object Handlers {
  def createPoll(command: CreatePollCommand, userId: Int): BotState = State(s => {
    val uid = UIDGen.next(s.lastuid)
    val visibility = command.visibility match {
      case None => Afterstop()
      case Some(v) => v
    }
    val anonymous = command.anonymous match {
      case Some(false) => false
      case _ => true
    }
    val poll = Poll(
        uid=uid,
        creatorId=userId,
        name=command.name,
        anonymous=anonymous,
        visibility=visibility,
        start=command.start,
        end=command.end,
    )
    (uid.toString, s.copy(polls=s.polls + (uid -> poll), lastuid=uid))
  })

  def list: BotState = State(s => s.polls.size match {
    case 0 => (Messages.noPolls, s)
    case _ => (s.polls.values.mkString("\n"), s)
  })

  def deletePoll(command: DeletePollCommand, userId: Int): BotState =
    State(s => s.polls.get(command.uid) match {
      case None => (Messages.noPoll(command.uid), s)
      case Some(poll) => userId == poll.creatorId match {
        case false => (Messages.notCreator, s)
        case true => (command.uid.toString, s.copy(polls=s.polls - command.uid))
      }
    })

  def startPoll(command: StartPollCommand, userId: Int): BotState =
    State(s => s.polls.get(command.uid) match {
      case None => (Messages.noPoll(command.uid), s)
      case Some(poll) => userId == poll.creatorId match {
        case false => (Messages.notCreator, s)
        case true => poll.finished match {
          case true => (Messages.pollAlreadyStopped, s)
          case false => poll.started match {
            case true => (Messages.pollAlreadyStarted, s)
            case false => poll.startSet match {
              case true => (Messages.pollWillStart, s)
              case false => (
                Messages.pollSuccessfullyStarted,
                s.copy(polls=s.polls + (command.uid -> poll.copy(start=Some(DateTime.now))))
              )
            }
          }
        }
      }
    })

  def stopPoll(command: StopPollCommand, userId: Int): BotState =
    State(s => s.polls.get(command.uid) match {
      case None => (Messages.noPoll(command.uid), s)
      case Some(poll) => userId == poll.creatorId match {
        case false => (Messages.notCreator, s)
        case true => poll.started match {
          case false => (Messages.pollNotStartedYet, s)
          case true => poll.finished match {
            case true => (Messages.pollAlreadyStopped, s)
            case false => poll.endSet match {
              case true => (Messages.pollWillStop, s)
              case false => (
                Messages.pollSuccessfullyStopped,
                s.copy(polls=s.polls + (command.uid -> poll.copy(end=Some(DateTime.now))))
              )
            }
          }
        }
      }
    })

  def result(command: ResultCommand): BotState = State(s => s.polls.get(command.uid) match {
    case None => (Messages.noPoll(command.uid), s)
    case Some(poll) => (poll.visibility, poll.finished) match {
      case (_:Afterstop, false) => (Messages.afterstopOnly, s)
      case _ => (poll + "\n\n" + poll.questions.reverse.zip(Stream.from(1)).map{
        case (q, i) => (q, poll.questionResult(i)) match {
          case (q:Open, Some(r:OpenResult)) => s"$i. $q\n$r"
          case (q:Choosable, Some(r:ChoiceResult)) => {
            s"$i. ${q.text} (${q.kind})\nTotal votes: ${r.totalVotes}\n" +
            q.options.zip(Stream.from(1)).map{
              case (o, i) => {
                val (count, users) = r.get(i)
                val sep = if (users.size > 0) "\n" else ""
                s"$i) $o – $count" + sep + users.mkString(", ")
              }
            }.mkString("\n")
          }
          case _ => ""
        }
      }.mkString("\n\n"), s)
    }
  })

  def begin(command: BeginCommand, userId: Int): BotState =
    State(s => s.polls.get(command.uid) match {
      case None => (Messages.noPoll(command.uid), s)
      case Some(poll) => (
        Messages.enterContext, s.copy(context=s.context + (userId -> command.uid))
      )
    })

  def end(userId: Int): BotState =
    State(s => s.context.get(userId) match {
      case None => (Messages.noContext, s)
      case Some(_) => (Messages.leaveContext, s.copy(context=s.context - userId))
    })

  def view(userId: Int): BotState = State(s => s.context.get(userId) match {
    case None => (Messages.noContext, s)
    case Some(uid) => s.polls.get(uid) match {
      case None => (Messages.noPoll(uid), s)
      case Some(poll) => (
        poll + poll.questions.reverse.zip(Stream.from(1)).map{
          case (q, i) => s"\n\n$i. $q"
        }.mkString, s
      )
    }
  })

  def addQuestion(command: AddQuestionCommand, userId: Int): BotState =
    State(s => s.context.get(userId) match {
      case None => (Messages.noContext, s)
      case Some(uid) => s.polls.get(uid) match {
        case None => (Messages.noPoll(uid), s)
        case Some(poll) => userId == poll.creatorId match {
          case false => (Messages.notCreator, s)
          case true => poll.started match {
            case true => (Messages.startedPollEdit, s)
            case false => {
              val question = command.kind match {
                case "open" => Open(command.question)
                case "choice" => Choice(command.question, command.options)
                case "multi" => Multi(command.question, command.options)
              }
              val newPoll = poll.copy(questions=question :: poll.questions)
              (newPoll.questions.length.toString, s.copy(polls=s.polls + (uid -> newPoll)))
            }
          }
        }
      }
    })

  def deleteQuestion(command: DeleteQuestionCommand, userId: Int): BotState =
    State(s => s.context.get(userId) match {
      case None => (Messages.noContext, s)
      case Some(uid) => s.polls.get(uid) match {
        case None => (Messages.noPoll(uid), s)
        case Some(poll) => userId == poll.creatorId match {
          case false => (Messages.notCreator, s)
          case true => poll.started match {
            case true => (Messages.startedPollEdit, s)
            case false => (command.n < 1 || command.n > poll.questions.length) match {
              case true => (Messages.noQuestion(command.n), s)
              case false => {
                val idx = poll.questions.length - command.n
                val questions = poll.questions.take(idx) ++ poll.questions.drop(idx+1)
                (Messages.questionSuccessfullyDeleted,
                  s.copy(polls=s.polls + (uid -> poll.copy(questions=questions))))
              }
            }
          }
        }
      }
    })

  def answer(command: AnswerCommand, user: User): BotState =
    State(s => s.context.get(user.id) match {
      case None => (Messages.noContext, s)
      case Some(uid) => s.polls.get(uid) match {
        case None => (Messages.noPoll(uid), s)
        case Some(poll) => poll.started match {
          case false => (Messages.pollNotStartedYet, s)
          case true => poll.finished match {
            case true => (Messages.pollAlreadyStopped, s)
            case false => poll.question(command.n) match {
              case None => (Messages.noQuestion(command.n), s)
              case Some(q) => poll.voters.getOrElse(command.n, Set()).contains(user.id) match {
                case true => (Messages.answerExists, s)
                case false => (q match {
                  case _:Open => Right(OpenAnswer(command.answer))
                  case q:Choice =>
                    CommandParser.parse(CommandParser.answerChoice, command.answer) match {
                      case CommandParser.Success(c, _) => q.correctChoice(c) match {
                        case false => Left(Messages.wrongChoice)
                        case true => Right(ChoiceAnswer(c))
                      }
                      case _ => Left(Messages.unknownCommand)
                    }
                  case q:Multi =>
                    CommandParser.parse(CommandParser.answerChoices, command.answer) match {
                      case CommandParser.Success(cs, _) => q.correctChoices(cs) match {
                        case false => Left(Messages.wrongChoice)
                        case true => Right(MultiAnswer(cs.toSet))
                      }
                      case _ => Left(Messages.unknownCommand)
                    }
                }) match {
                  case Left(m) => (m, s)
                  case Right(a) => {
                    val answer = (poll.anonymous match {
                      case true => (a, None)
                      case false => (a, Some(user))
                    }) :: poll.answers.getOrElse(command.n, List())
                    val vote = command.n -> (poll.voters.getOrElse(command.n, Set()) + user.id)
                    val newPoll = poll.copy(
                      answers=poll.answers + (command.n -> answer),
                      voters=poll.voters + vote
                    )
                    (Messages.answerSaved, s.copy(polls=s.polls + (uid -> newPoll)))
                  }
                }
              }
            }
          }
        }
      }
    })
}