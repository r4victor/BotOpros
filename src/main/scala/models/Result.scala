package botopros.models

sealed abstract class Result

case class OpenResult(stat: List[(String, Option[User])] = List()) extends Result {
  override def toString: String =
    s"Answers: ${stat.length}" +
    (if (stat.length > 0) "\n" else "") +
    stat.map{
      case (s, Some(u)) => s"$s – $u"
      case (s, _) => s"$s"
    }.mkString("\n")
}

case class ChoiceResult(stat: Map[Int, (Int, Set[User])] = Map()) extends Result {
  def get(n: Int): (Int, Set[User]) = stat.getOrElse(n, (0, Set()))

  def totalVotes: Int = stat.foldLeft(0){
    case (acc, (_, (i,_))) => acc + i
  }
}