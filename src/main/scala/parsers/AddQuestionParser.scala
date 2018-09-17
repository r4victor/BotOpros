package botopros.parsers

import botopros.models.AddQuestionCommand

trait AddQuestionParser extends CommandParsers {
  def addQuestion: Parser[AddQuestionCommand] =  phrase(open) | phrase(choosable)

  def open: Parser[AddQuestionCommand] =
    "/add_question" ~> arg(string) ~ opt(arg("open")) ^^ {
      case q ~ None => AddQuestionCommand(q, "open")
      case q ~ Some(c) => AddQuestionCommand(q, c)
    }

  def choosable: Parser[AddQuestionCommand] =
    "/add_question" ~> arg(string) ~ arg("choice" | "multi") ~ option.+ ^^ {
      case q ~ c ~ os => AddQuestionCommand(q, c, os)
    }

  def option: Parser[String] = escapeHTML(".+".r)
}