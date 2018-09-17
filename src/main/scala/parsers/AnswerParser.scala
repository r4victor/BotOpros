package botopros.parsers

import botopros.models.AnswerCommand

trait AnswerParser extends CommandParsers {
  def answer: Parser[AnswerCommand] = "/answer" ~> uid ~ arg(string) ^^ {
      case n ~ s => AnswerCommand(n, s)
    }

  def answerChoice: Parser[Int] = phrase(choice)

  def answerChoices: Parser[List[Int]] = rep1(choice)

  def choice: Parser[Int] = "\\d+".r ^^ { _.toInt }
}