package botopros.parsers

import botopros.models._
import botopros.helpers.DateTime

trait CreatePollParser extends CommandParsers {
  def poll: Parser[CreatePollCommand] =
    "/create_poll" ~> phrase(fiveArgs | fourArgs | threeArgs | twoArgs | oneArg)

  def oneArg: Parser[CreatePollCommand] = arg(string) ^^ {
    case name => CreatePollCommand(name)
  }

  def twoArgs: Parser[CreatePollCommand] =
    (arg(string) ~ arg(anonymous)) ^^ {
    case name ~ a => CreatePollCommand(name, Some(a))
  }

  def threeArgs: Parser[CreatePollCommand] =
    (arg(string) ~ arg(anonymous) ~ arg(visibility)) ^^ {
    case name ~ a ~ v => CreatePollCommand(name, Some(a), Some(v))
  }

  def fourArgs: Parser[CreatePollCommand] =
    (arg(string) ~ arg(anonymous) ~ arg(visibility) ~ arg(date)) ^^ {
      case name ~ a ~ v ~ s => CreatePollCommand(name, Some(a), Some(v), Some(s))
    }

  def fiveArgs: Parser[CreatePollCommand] =
    (arg(string) ~ arg(anonymous) ~ arg(visibility) ~ arg(date) ~ arg(date)) ^^ {
      case name ~ a ~ v ~ s ~ e => CreatePollCommand(name, Some(a), Some(v), Some(s), Some(e))
    }

  def anonymous: Parser[Boolean] = ("yes" | "no") ^^ {
    case x => x == "yes"
  }

  def visibility: Parser[PollVisibility] = ("afterstop" | "continuous") ^^ {
    case "afterstop" => Afterstop()
    case "continuous" => Continuous()
  }

  def date: Parser[Long] = new Parser[Long] {
    def apply(in: Input): ParseResult[Long] = {
      val offset = in.offset
      val start = handleWhiteSpace(in.source, offset)
      val ws = start - offset
      val formatLen = DateTime.formatString.length
      val end = start + formatLen
      if (end >= in.source.length) return Failure("", in.drop(ws))
      val substr = in.source.subSequence(start, end).toString
      DateTime.parse(substr) match {
        case Some(d) => Success(d, in.drop(formatLen + ws))
        case _ => Failure("", in.drop(ws))
      }
    }
  }
}