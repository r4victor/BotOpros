package botopros.handlers

import org.scalatest.{FunSpec, GivenWhenThen}

import botopros.models._
import botopros.helpers.DateTime
import Handlers._

class HandlersSpec extends FunSpec with GivenWhenThen {
  val pollName = "Some poll"
  val userId = 123456
  val userId1 = userId + 1
  val pollId = 1435
  val date = DateTime.now
  val question  = "how a u?"
  val sampleAnswer = "fine"

  describe("/list handler") {
    it("should inform that no polls exist") {
      Given("an empty state")
      val s = StateData()

      When("command is invoked")
      val res = list.run(s)

      Then("inform")
      assertResult((Messages.noPolls, s)) {
        res
      }
    }

    it("should list all existing polls") {
      Given("a state with three polls")
      val poll1 = Poll(1,1,"1")
      val poll2 = Poll(2,2,"2")
      val poll3 = Poll(3,3,"3")
      val s = StateData(Map(1 -> poll1, 2 -> poll2, 3 -> poll3))

      When("command is invoked")
      val res = list.run(s)

      Then("list three polls")
      assertResult((s"$poll1\n$poll2\n$poll3", s)) { res }
    }
  }

  describe("/create_poll handler") {

    it("should create a new poll with default arguments") {
      Given("an empty state")
      val s = StateData()

      When("command with a poll name argument is invoked by some user")
      val (m, newS) = createPoll(CreatePollCommand(pollName), userId).run(s)

      Then("update state with a new poll")
      assertResult(Map(1 -> Poll(1, userId, pollName))) { newS.polls }

      And("update last poll id")
      assertResult(1) { newS.lastuid }

      And("return poll id")
      assertResult("1") { m }
    }
  }

  describe("/delete_poll handler") {
    it("should delete the poll if it exists and command is invoked by the creator") {
      Given("a state that contains a poll")
      val s = StateData(Map(pollId -> Poll(pollId, userId, pollName)))

      When("command with a poll id argument is invoked by the creator")
      val (m, newS) = deletePoll(DeletePollCommand(pollId), userId).run(s)

      Then("delete the poll")
      assertResult(StateData()) { newS }
    }

    it("shouldn't delete the poll if command is invoked not by the creator") {
      Given("a state that contains a poll")
      val s = StateData(Map(pollId -> Poll(pollId, userId, pollName)))

      When("command with a poll id argument is invoked not by the creator")
      val (m, newS) = deletePoll(DeletePollCommand(pollId), userId + 1).run(s)

      Then("don't delete the poll")
      assertResult(s) { newS }
    }

    it("should inform if the poll doesn't exist") {
      Given("an empty state")
      val s = StateData()

      When("command with a poll id argument is invoked")
      val res = deletePoll(DeletePollCommand(pollId), userId).run(s)

      Then("inform")
      assertResult((Messages.noPoll(pollId), s)) { res }
    }
  }

  describe("/start_poll handler") {
    describe("when command is invoked by the creator of the poll") {
      it("should start the poll if start date is not defined") {
        Given("a state that contains a poll with undefined start date")
        val s = StateData(Map(pollId -> Poll(pollId, userId, pollName)))

        When("command with a poll id argument is invoked")
        val (m, newS) = startPoll(StartPollCommand(pollId), userId).run(s)

        Then("update poll start date with a current time")
        assert(newS.polls.get(pollId).get.start.get == DateTime.now)
      }

      it("shouldn't update the poll if start date is defined") {
        Given("a state that contains a poll with defined start date")
        val s = StateData(
          Map(pollId -> Poll(pollId, userId, pollName, start=Some(date)))
        )

        When("command with a poll id argument is invoked")
        val (m, newS) = startPoll(StartPollCommand(pollId), userId).run(s)

        Then("keep the poll as it is")
        assertResult(s) { newS }

        And("inform with a non-empty message")
        assert(m != "")
      }
    }

    describe("when command is invoked not by the creator of the poll") {
      it("should never start the poll") {
        Given("a state that contains a poll with undefined start date")
        val s = StateData(Map(pollId -> Poll(pollId, userId, pollName)))

        When("command with a poll id argument is invoked")
        val (m, newS) = startPoll(StartPollCommand(pollId), userId + 1).run(s)

        Then("keep the poll as it is")
        assertResult(s) { newS }

        And("inform")
        assertResult(Messages.notCreator) { m }
      }
    }
  }

  describe("/stop_poll handler") {
    describe("when command is invoked by the creator of the poll") {
      it("shouldn't stop the poll if start date is not defined") {
        Given("a state that contains a poll with defined start date and undefined stop date")
        val s = StateData(Map(pollId -> Poll(pollId, userId, pollName)))

        When("command with a poll id argument is invoked")
        val (m, newS) = stopPoll(StopPollCommand(pollId), userId).run(s)

        Then("keep the poll as it is")
        assertResult(s) { newS }

        And("inform")
        assertResult(Messages.pollNotStartedYet) { m }
      }

      it("shouldn't stop the poll if poll hasn't been started") {
        Given("a state that contains a poll which hasn't been started yet")
        val s = StateData(
          Map(pollId -> Poll(pollId, userId, pollName, start=Some(DateTime.now+1000)))
        )

        When("command with a poll id argument is invoked")
        val (m, newS) = stopPoll(StopPollCommand(pollId), userId).run(s)

        Then("keep the poll as it is")
        assertResult(s) { newS }

        And("inform")
        assertResult(Messages.pollNotStartedYet) { m }
      }

      it("should stop the poll if stop date is not defined and poll has started") {
        Given("a state that contains a poll which has been started")
        val s = StateData(
          Map(pollId -> Poll(pollId, userId, pollName, start=Some(DateTime.now-1000)))
        )

        When("command with a poll id argument is invoked")
        val (m, newS) = stopPoll(StopPollCommand(pollId), userId).run(s)

        Then("update poll stop date with a current time")
        assert(newS.polls.get(pollId).get.end.get == DateTime.now)

        And("inform that the poll has been stopped successfully")
        assertResult(Messages.pollSuccessfullyStopped) { m }
      }
    }

    describe("when command is invoked not by the creator of the poll") {
      it("should never stop the poll") {
        Given("a state that contains a started poll")
        val s = StateData(
          Map(pollId -> Poll(pollId, userId, pollName, start=Some(DateTime.now-1000)))
        )

        When("command with a poll id argument is invoked")
        val (m, newS) = stopPoll(StopPollCommand(pollId), userId + 1).run(s)

        Then("keep the poll as it is")
        assertResult(s) { newS }

        And("inform")
        assertResult(Messages.notCreator) { m }
      }
    }

    describe("/result handler") {
      val poll = Poll(
        pollId, userId, pollName,
        visibility=Continuous(),
        start=Some(DateTime.now-1000),
        questions=List(Open(question)),
        answers=Map(1 -> List((OpenAnswer(sampleAnswer), None)))
      )

      it("should show the results if visibility type is Continuous") {
        Given("a state that contains a poll with Continuous() as a visibility value")
        val s = StateData(Map(pollId -> poll))

        When("command with a poll id argument is invoked")
        val (m, newS) = result(ResultCommand(pollId)).run(s)

        Then("show results")
        assertResult(s"$poll\n\n1. $question (open)\nAnswers: 1\n$sampleAnswer") { m }
      }

      it("should show the results if visibility type is Afterstop and poll has been stopped") {
        Given("a state that contains a finished poll with Afterstop() as a visibility value")
        val poll1 = poll.copy(visibility=Afterstop(), end=Some(DateTime.now-500))
        val s = StateData(Map(pollId -> poll1))

        When("command with a poll id argument is invoked")
        val (m, newS) = result(ResultCommand(pollId)).run(s)

        Then("show results")
        assertResult(s"$poll1\n\n1. $question (open)\nAnswers: 1\n$sampleAnswer") { m }
      }
    }

    describe("/begin handler") {
      it("should start working with an existing poll") {
        Given("a state that contains a poll")
        val s = StateData(Map(pollId -> Poll(pollId, userId, pollName)))

        When("command with a poll id argument is invoked")
        val (m, newS) = begin(BeginCommand(pollId), userId1).run(s)

        Then("update the context with that poll")
        assertResult(Map(userId1 -> pollId)) { newS.context }
      }

      it("shouldn't change the context if poll doesn't exist") {
        Given("an empty state")
        val s = StateData()

        When("command with a poll id argument is invoked")
        val (m, newS) = begin(BeginCommand(pollId), userId).run(s)

        Then("keep the context as it is")
        assertResult(s.context) { newS.context }

        And("inform that the poll doesn't exist")
        assertResult(Messages.noPoll(pollId)) { m }
      }
    }

    describe("/end handler") {
      it("should stop working with a poll if user is working with any") {
        Given("a state that contains a poll and a non-empty context")
        val s = StateData(
          Map(pollId -> Poll(pollId, userId, pollName)),
          Map(userId1 -> pollId)
        )

        When("command is invoked by user who is working with a poll")
        val (m, newS) = end(userId1).run(s)

        Then("delete the corresponding (user -> poll) record from the context")
        assertResult(Map()) { newS.context }
      }

      it("shouldn't change the context if user isn't working with a poll") {
        Given("a state that contains a poll and a non-empty context")
        val s = StateData(
          Map(pollId -> Poll(pollId, userId, pollName)),
          Map(userId1 -> pollId)
        )

        When("command is invoked by user who is not working with a poll")
        val (m, newS) = end(userId).run(s)

        Then("keep the context as it is")
        assertResult(Map(userId1 -> pollId)) { newS.context }

        And("inform that user is not working with any poll")
        assertResult(Messages.noContext) { m }
      }
    }

    describe("/view hanlder") {
      val poll = Poll(
          pollId, userId, pollName,
          questions=List(Choice("where?", List("here", "there")), Open("what?"))
        )
      val pollView = s"#$pollId – $pollName – anonymous poll, not started\n\n" +
        "1. what? (open)\n\n2. where? (choice)\n1) here\n2) there"

      it("should show the poll if user is working with any") {
        Given("a state that contains a poll and a non-empty context")
        val s = StateData(Map(pollId -> poll), Map(userId -> pollId))

        When("command is invoked by user who is working a the poll")
        val (m, newS) = view(userId).run(s)

        Then("show the poll")
        assertResult(pollView) { m }
      }

      it("shouldn't show the poll if user is not working with any") {
        Given("a state that contains a poll and a non-empty context")
        val s = StateData(Map(pollId -> poll), Map(userId -> pollId))

        When("command is invoked by user who is not working with a poll")
        val (m, newS) = view(userId1).run(s)

        Then("inform that user is not working with any poll")
        assertResult(Messages.noContext) { m }
      }
    }

    describe("/add_question handler") {
      val poll = Poll(pollId, userId, pollName)
      val command = AddQuestionCommand(question, "open")

      describe("when command is invoked by user who is not working with any poll") {
        it("should decline an attempt to add the question") {
          Given("a state that contains a poll and a non-empty context")
          val s = StateData(Map(pollId -> poll), Map(userId -> pollId))

          When("command is invoked by user who is not working with a poll")
          val (m, newS) = addQuestion(command, userId1).run(s)

          Then("don't change the state")
          assertResult(s) { newS }

          And("inform that user is not working with any poll")
          assertResult(Messages.noContext) { m }
        }
      }

      describe("when command is invoked by user who is working with some poll") {
        describe("when user is not the creator of the poll") {
          it("should decline an attempt to add the question") {
            Given("a state that contains a poll and a non-empty context")
            val s = StateData(Map(pollId -> poll), Map(userId1 -> pollId))

            When("command is invoked not by the creator of the poll")
            val (m, newS) = addQuestion(command, userId1).run(s)

            Then("don't change the state")
            assertResult(s) { newS }

            And("inform that only creator of the poll can edit one")
            assertResult(Messages.notCreator) { m }
          }
        }

        describe("when user is the creator of the poll") {
          describe("when poll has been started") {
            it("should decline an attempt to add the question") {
              Given("a state that contains a started poll and a non-empty context")
              val startedPoll = poll.copy(start=Some(DateTime.now-1000))
              val s = StateData(Map(pollId -> startedPoll), Map(userId -> pollId))

              When("command is invoked by the creator of the poll working with it")
              val (m, newS) = addQuestion(command, userId).run(s)

              Then("don't change the state")
              assertResult(s) { newS }

              And("inform that user cannot edit a started poll")
              assertResult(Messages.startedPollEdit) { m }
            }
          }

          describe("when poll hasn't been started yet") {
            it("should add the question to the poll") {
              Given("a state that contains a poll that hasn't been started yet")
              val notStartedPoll = poll.copy(start=Some(DateTime.now+1000))
              val s = StateData(Map(pollId -> notStartedPoll), Map(userId -> pollId))

              When("command is invoked by the creator of the poll working with it")
              val (m, newS) = addQuestion(command, userId).run(s)

              Then("add the question")
              assertResult(notStartedPoll.copy(questions=List(Open(question)))) {
                newS.polls.get(pollId).get
              }
            }
          }
        }
      }
    }

    describe("/delete_question handler") {
      val poll = Poll(pollId, userId, pollName, questions=List(Open(question)))
      val command = DeleteQuestionCommand(1)

      describe("when command is invoked by user who is not working with any poll") {
        it("should decline an attempt to delete the question") {
          Given("a state that contains a poll and a non-empty context")
          val s = StateData(Map(pollId -> poll), Map(userId -> pollId))

          When("command is invoked by user who is not working with a poll")
          val (m, newS) = deleteQuestion(command, userId1).run(s)

          Then("don't change the state")
          assertResult(s) { newS }

          And("inform that user is not working with any poll")
          assertResult(Messages.noContext) { m }
        }
      }

      describe("when command is invoked by user who is working with some poll") {
        describe("when user is not the creator of the poll") {
          it("should decline an attempt to delete the question") {
            Given("a state that contains a poll and a non-empty context")
            val s = StateData(Map(pollId -> poll), Map(userId1 -> pollId))

            When("command is invoked not by the creator of the poll")
            val (m, newS) = deleteQuestion(command, userId1).run(s)

            Then("don't change the state")
            assertResult(s) { newS }

            And("inform that only creator of the poll can edit one")
            assertResult(Messages.notCreator) { m }
          }
        }

        describe("when user is the creator of the poll") {
          describe("when poll has been started") {
            it("should decline an attempt to delete the question") {
              Given("a state that contains a started poll and a non-empty context")
              val startedPoll = poll.copy(start=Some(DateTime.now-1000))
              val s = StateData(Map(pollId -> startedPoll), Map(userId -> pollId))

              When("command is invoked by the creator of the poll working with it")
              val (m, newS) = deleteQuestion(command, userId).run(s)

              Then("don't change the state")
              assertResult(s) { newS }

              And("inform that user cannot delete the question of a started poll")
              assertResult(Messages.startedPollEdit) { m }
            }
          }

          describe("when poll hasn't been started yet") {
            it("should delete the question") {
              Given("a state that contains a poll that hasn't been started yet")
              val notStartedPoll = poll.copy(end=Some(DateTime.now+1000))
              val s = StateData(Map(pollId -> notStartedPoll), Map(userId -> pollId))

              When("command is invoked by the creator of the poll working with it")
              val (m, newS) = deleteQuestion(command, userId).run(s)

              Then("delete the question")
              assertResult(notStartedPoll.copy(questions=List())) {
                newS.polls.get(pollId).get
              }
            }
          }
        }
      }
    }

    describe("/answer handler") {
      val command = AnswerCommand(1, sampleAnswer)
      val user = User(userId, "Mika", Some("Palacher"))
      val poll = Poll(pollId, userId, pollName, questions=List(Open(question)))
      val notStartedPoll = poll.copy(start=Some(DateTime.now+1000))
      val activePoll = poll.copy(
        start=Some(DateTime.now-1000),
        end=Some(DateTime.now+1000)
      )
      val stoppedPoll = poll.copy(
        start=Some(DateTime.now-1000),
        end=Some(DateTime.now-500)
      )

      describe("when command is invoked by user who is not working with any poll") {
        it("should decline an attempt to answer the question") {
          Given("a state that contains a poll and a non-empty context")
          val s = StateData(Map(pollId -> poll), Map(userId -> pollId))

          When("command is invoked by user who is not working with a poll")
          val (m, newS) = answer(command, user.copy(id=userId1)).run(s)

          Then("don't change the state")
          assertResult(s) { newS }

          And("inform that user is not working with any poll")
          assertResult(Messages.noContext) { m }
        }
      }

      describe("when command is invoked by user who is working with some poll") {
        it("should decline an attempt to answer the question that doesn't exist") {
          Given("a state that contains an active poll")
          val s = StateData(Map(pollId -> activePoll), Map(userId -> pollId))

          When("command is invoked by user working with the poll")
          val (m, newS) = answer(AnswerCommand(2, sampleAnswer), user).run(s)

          Then("don't change the state")
          assertResult(s) { newS }

          And("inform that the question doesn't exits")
          assertResult(Messages.noQuestion(2)) { m }
        }


        it("should decline an attempt to answer the question if poll hasn't been started") {
          Given("a state that contains a poll that hasn't been started yet")
          val s = StateData(Map(pollId -> notStartedPoll), Map(userId -> pollId))

          When("command is invoked by user working with the poll")
          val (m, newS) = answer(command, user).run(s)

          Then("don't change the state")
          assertResult(s) { newS }

          And("inform that user cannot answer the question if poll hasn't been started")
          assertResult(Messages.pollNotStartedYet) { m }
        }

        it("should decline an attempt to answer the question if poll has been stopped") {
          Given("a state that contains a poll that has been stopped")
          val s = StateData(Map(pollId -> stoppedPoll), Map(userId -> pollId))

          When("command is invoked by user working with the poll")
          val (m, newS) = answer(command, user).run(s)

          Then("don't change the state")
          assertResult(s) { newS }

          And("inform that user cannot answer the question if poll has been stopped")
          assertResult(Messages.pollAlreadyStopped) { m }
        }

        describe("when poll is active") {
          it("should save an answer without user's info if the poll is anonymous") {
            Given("a state that contains an anonymous poll")
            val s = StateData(Map(pollId -> activePoll), Map(userId -> pollId))

            When("command is invoked by user working with the poll")
            val (m, newS) = answer(command, user).run(s)

            Then("save the answer without user's info")
            assertResult((OpenAnswer(sampleAnswer), None)) {
              newS.polls.get(pollId).get.answers.get(1).get(0)
            }
          }

          it("should save an answer with user's info if the poll is pulbic") {
            Given("a state that contains a public poll")
            val s = StateData(
              Map(pollId -> activePoll.copy(anonymous=false)),
              Map(userId -> pollId)
            )

            When("command is invoked by user working with the poll")
            val (m, newS) = answer(command, user).run(s)

            Then("save the answer and user's info")
            assertResult((OpenAnswer(sampleAnswer), Some(user))) {
              newS.polls.get(pollId).get.answers.get(1).get(0)
            }
          }

          describe("when question's type is choice") {
            it("should save an answer if it's a single option number") {
              Given("a state that contains a poll with question of type choice")
              val s = StateData(
                Map(pollId -> activePoll.copy(
                  questions=List(Choice(question, List("op1", "op2"))))),
                Map(userId -> pollId)
              )

              When("command with correct option number is invoked by user working with the poll")
              val (m, newS) = answer(AnswerCommand(1, "2"), user).run(s)

              Then("save the answer")
              assertResult((ChoiceAnswer(2), None)) {
                newS.polls.get(pollId).get.answers.get(1).get(0)
              }
            }

            it("shouldn't save an answer if it's a sequence of numbers") {
              Given("a state that contains a poll with question of type choice")
              val s = StateData(
                Map(pollId -> activePoll.copy(
                  questions=List(Choice(question, List("op1", "op2"))))),
                Map(userId -> pollId)
              )

              When("command with multiple numbers is invoked by user working with the poll")
              val (m, newS) = answer(AnswerCommand(1, "1 2"), user).run(s)

              Then("reject the answer")
              assert(newS.polls.get(pollId).get.answers.size == 0)
            }
          }

          describe("when question's type is multi") {
            it("should save an answer if it's a sequence of numbers") {
              Given("a state that contains a poll with question of type multi")
              val s = StateData(
                Map(pollId -> activePoll.copy(
                  questions=List(Multi(question, List("op1", "op2"))))),
                Map(userId -> pollId)
              )

              When("command with multiple numbers is invoked by user working with the poll")
              val (m, newS) = answer(AnswerCommand(1, "1 2"), user).run(s)

              Then("save the answer and count the option once")
              assertResult((MultiAnswer(Set(1, 2)), None)) {
                newS.polls.get(pollId).get.answers.get(1).get(0)
              }
            }
          }
        }
      }
    }
  }
}