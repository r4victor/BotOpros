package botopros.models

import botopros.handlers.Messages
import botopros.helpers.DateTime

case class Poll(
  uid: Int,
  creatorId: Int,
  name: String,
  anonymous: Boolean = true,
  visibility: PollVisibility = Afterstop(),
  start: Option[Long] = None,
  end: Option[Long] = None,
  questions: List[Question] = List(),
  answers: Map[Int, List[(Answer, Option[User])]] = Map(),
  voters: Map[Int, Set[Int]] = Map()
) {
  def active: Boolean = started && !finished

  def started: Boolean = start match {
    case None => false
    case Some(s) => DateTime.now >= s
  }

  def finished: Boolean = end match {
    case None => false
    case Some(e) => DateTime.now >= e
  }

  def startSet: Boolean = start match {
    case None => false
    case _ => true
  }

  def endSet: Boolean = end match {
    case None => false
    case _ => true
  }

  def question(n: Int): Option[Question] =
    this.questions.lift(this.questions.length - n)

  def questionResult(n: Int): Option[Result] = {
    def openResult(answers: List[(Answer, Option[User])]): Option[OpenResult] = answers match {
      case Nil => Some(OpenResult())
      case (a: OpenAnswer, o) :: t => openResult(t) match {
        case None => None
        case Some(r) => Some(OpenResult((a.text, o) :: r.stat))
      }
      case _ => None
    }

    def choiceResult(answers: List[(Answer, Option[User])]): Option[ChoiceResult] = answers match {
      case Nil => Some(ChoiceResult())
      case (a: ChoiceAnswer, o) :: t => choiceResult(t) match {
        case None => None
        case Some(r) => {
          val (count, users): (Int, Set[User]) = r.stat.getOrElse(a.choice, (0, Set()))
          val updatedUsers = o match {
            case None => users
            case Some(u) => users + u
          }
          Some(ChoiceResult(r.stat + (a.choice -> (count + 1, updatedUsers))))
        }
      }
      case _ => None
    }

    def multiResult(answers: List[(Answer, Option[User])]): Option[ChoiceResult] = answers match {
      case Nil => Some(ChoiceResult())
      case (a: MultiAnswer, o) :: t => choiceResult(t) match {
        case None => None
        case Some(r) => Some(ChoiceResult(r.stat ++ a.choices.map(choice => {
          val (count, users): (Int, Set[User]) = r.stat.getOrElse(choice, (0, Set()))
          val updatedUsers = o match {
            case None => users
            case Some(u) => users + u
          }
          choice -> (count + 1, updatedUsers)
        })))
      }
      case _ => None
    }

      question(n) match {
      case None => None
      case Some(q) => {
        val answerList = answers.getOrElse(n, List())
        q match {
          case q:Open => openResult(answerList)
          case q:Choice => choiceResult(answerList)
          case q:Multi => multiResult(answerList)
        }
      }
    }
  }

  override def toString: String = {
    val status = (active, started, finished) match {
      case (true,_,_) => "active"
      case (_, false, false) => "not started"
      case _ => "finished"
    }
    val anon = anonymous match {
      case false => "public"
      case true => "anonymous"
    }
    s"#$uid – $name – $anon poll, $status"
  }
}