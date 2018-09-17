package botopros.parsers

import scala.util.parsing.combinator._

import botopros.models._

trait CommandParsers extends RegexParsers {
  def string: Parser[String] = escapeHTML("([^()]|\\(\\(|\\)\\))+".r ^^ {
    arg => arg.replace("((", "(").replace("))", ")")
  })

  def escapeHTML(p: Parser[String]): Parser[String] = p ^^ {
    arg => arg.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
  }

  def arg[U](p: => Parser[U]) = "(" ~> p <~ ")"

  def primitive(s: String, c: Command): Parser[Command] = phrase(s ^^ (_ => c))

  def uid: Parser[Int] = arg("\\d+".r ^^ {_.toInt})

  def uidCommand(s: String, f: Int => Command): Parser[Command] = s ~> uid ^^ f
}