package botopros.handlers

object Messages {
  import CommandDescriptions._

  val help = escapeHTML(
    "I understand the following commands:\n\n" +
    List(list, createPoll, delete, startPoll, stopPoll,
         result, begin, end, view, addQuestion, deleteQuestion, answer
    ).mkString("\n") +
    "\n\n<> - required parameters\n[] - optional parameters\n" +
    "date format: hh:mm:ss yy:MM:dd\n" +
    "Parameters have to be surrounded by parenthesis: (<param>)\n" +
    "Parenthesis themselves are escaped by doubling: (("
  )
  val noCommand = "No command. " + help
  val unknownCommand = "Bad syntax. " + help
  val noUser = "No user"

  val noPolls = "No polls have been created yet"
  val noContext = escapeHTML("You haven't choose a poll. Send /begin <poll id:num>")
  def noPoll(id: Int) = s"Poll with id $id doesn't exist"
  val notCreator = "Only the creator of the poll is able to edit it"
  val enterContext = "Now you can work with the poll!"
  val leaveContext = ""

  val pollAlreadyStarted = "Poll has been already started"
  val pollWillStart = "Poll will be started automatically"
  val pollNotStartedYet = "Poll hasn't been started yet"
  val pollSuccessfullyStarted = "Poll is started"

  val pollAlreadyStopped = "Poll has been stopped already"
  val pollWillStop = "Poll will be stopped automatically"
  val pollSuccessfullyStopped = "Poll is stopped"

  val afterstopOnly = "Result will be available after stop"
  def noQuestion(n: Int) = s"Question #$n doesn't exist"
  val startedPollEdit = "Started poll cannot be edited"
  val questionSuccessfullyDeleted = "Question was deleted"
  val answerExists = "You've answered that question"
  val wrongChoice = "Wrong option numbers"
  val answerSaved = "Answer is saved"

  def escapeHTML(str: String): String =
    str.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
}

object CommandDescriptions {
  val createPoll= "/create_poll <name:string> [anonymuous:yes|no] " + 
    "[result visibility:afterstop|continuous] [start:date] [end:date] – create a new poll"
  val list = "/list – list all polls"
  val delete = "/delete_poll <poll id:num> – delete an existing poll"
  val startPoll = "/start_poll <poll id:num> – start poll manually"
  val stopPoll = "/stop_poll <poll id:num> – stop poll manually"
  val result = "/result <poll id:num> – show poll result"
  val begin = "/begin <poll id:num> – choose a poll to work with"
  val end = "/end – stop working with the choosen poll"
  val view = "/view – show the choosen poll"
  val addQuestion = "/add_question <question:string> [type:open|choice|multi]" +
    "\noption1\noption2\n... – add a new question to the choosen poll"
  val deleteQuestion = "/delete_question <question number:num> – delete the question with a given number"
  val answer = "/answer <question number:num> <answer:string> – answer the question with a given number"
}