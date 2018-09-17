package botopros.models

case class User(id: Int, firstName: String, lastName: Option[String]) {
  def lastNameRepr: String = lastName match {
    case Some(l) => l
    case None => "" 
  }

  override def toString: String = s"<a href='tg://user?id=$id'>$firstName $lastNameRepr</a>"
}