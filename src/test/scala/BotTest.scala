package botopros

import org.scalatest.{FunSpec, GivenWhenThen}

import botopros.helpers._
import botopros.handlers._
import botopros.models._

class BotTest extends FunSpec with GivenWhenThen {
  describe("Bot") {
    def invoke(command: String, user: User, stateData: StateData) =
      Dispatcher.handle(command, user).run(stateData)

    it("should behave as follows") {
      Given("initial state and two users")
      val s = StateData()
      val user1 = User(1234, "First", None)
      val user2 = User(5234, "Second", None)

      When("User1 creates a public continuous poll")
      val (m1, s1) = invoke("/create_poll (Movies) (no) (continuous)", user1, s)

      Then("should create the poll")
      val pollId = UIDGen.next(s.lastuid)
      val poll = Poll(pollId, user1.id, "Movies", false, Continuous())
      assertResult(poll) {
       s1.polls.get(pollId).get
      }

      And("return poll id")
      assertResult(pollId.toString) { m1 }

      When("User2 starts working with the poll")
      val (m2, s2) = invoke(s"/begin ($pollId)", user2, s1)

      And("adds a question")
      val (m3, s3) = invoke(s"/add_question (How old are you?) (open)", user2, s2)

      Then("should reject an attempt")
      assertResult(poll) {
        s3.polls.get(pollId).get
      }

      And("inform User2 that he is not the creator")
      assertResult(Messages.notCreator) { m3 }

      When("User1 starts working with the poll")
      val (m4, s4) = invoke(s"/begin ($pollId)", user1, s3)

      And("adds a question")
      val (m5, s5) = invoke(s"/add_question (How old are you?) (open)", user1, s4)

      Then("should save it")
      val question = Open("How old are you?")
      val poll1 = poll.copy(questions=List(question))
      assertResult(poll1) {
       s5.polls.get(pollId).get
      }

      When("User2 answer the question")
      val (m6, s6) = invoke(s"/answer (1) (23)", user2, s5)

      Then("should reject an attempt")
      assertResult(poll1) {
        s6.polls.get(pollId).get
      }

      When("User1 starts the poll")
      val (m7, s7) = invoke(s"/start_poll ($pollId)", user1, s6)

      And("User2 answer the question")
      val (m8, s8) = invoke(s"/answer (1) (23)", user2, s7)

      Then("should save it")
      assertResult(Map(1 -> List((OpenAnswer("23"), Some(user2))))) {
        s8.polls.get(pollId).get.answers
      }
    }
  }
}