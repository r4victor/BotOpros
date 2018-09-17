package botopros.models

sealed trait Question {
  def text: String
  def kind: String
}

sealed trait Choosable extends Question {
  def options: List[String]

  def numberedOptions: List[String] =
    options.zip(Stream.from(1)).map{case (o, i) => s"$i) $o"}

  override def toString: String =
    s"$text ($kind)\n" + numberedOptions.mkString("\n")
}

case class Open(text: String) extends Question {
  override def kind: String = "open"

  override def toString: String = s"$text ($kind)"
}

case class Choice(text: String, options: List[String]) extends Choosable {
  def correctChoice(choice: Int): Boolean =
    1 <= choice && choice <= options.length

  override def kind: String = "choice"
}

case class Multi(text: String, options: List[String]) extends Choosable {
  def correctChoices(choices: List[Int]): Boolean =
    choices.forall(choice => 1 <= choice && choice <= options.length)

  override def kind: String = "multi"
}