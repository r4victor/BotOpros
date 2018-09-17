package botopros.parsers

import org.scalatest.FunSpec

import botopros.models._

class CommandParserSpec extends FunSpec {
  describe("A CommandParser") {
    val uid = 123
    val n = 12

    describe("on /create_poll command") {
      val name = "some poll"

      it("should return a CreatePollCommand when given a name") {
        assertResult(CreatePollCommand(name)) {
          CommandParser(s"/create_poll ($name)").getOrElse(null)
        }
      }

      it("should require a name") {
        assert(CommandParser(s"/create_poll").isEmpty)
      }

      it("should require the parentheses around name") {
        assert(CommandParser(s"/create_poll $name").isEmpty)
      }

      it("should reject the names that contain unescaped parentheses") {
        assert(CommandParser(s"/create_poll ())").isEmpty)
      }

      describe("when anonymity argument is presented") {
        it("should recognize yes") {
          assertResult(CreatePollCommand(name, Some(true))) {
            CommandParser(s"/create_poll ($name) (yes)").getOrElse(null)
          }
        }

        it("should recognize no") {
          assertResult(CreatePollCommand(name, Some(false))) {
            CommandParser(s"/create_poll ($name) (no)").getOrElse(null)
          }
        }

        it("should reject other options") {
          assert(CommandParser(s"/create_poll ($name) (ok)").isEmpty)
        }

        it("should require the parentheses") {
          assert(CommandParser(s"/create_poll ($name) no").isEmpty)
        }
      }

      describe("when visibility argument is presented") {
        it("should recognize afterstop") {
          assertResult(CreatePollCommand(name, Some(true), Some(Afterstop()))) {
            CommandParser(s"/create_poll ($name) (yes) (afterstop)").getOrElse(null)
          }
        }

        it("should recognize continuous") {
          assertResult(CreatePollCommand(name, Some(true), Some(Continuous()))) {
            CommandParser(s"/create_poll ($name) (yes) (continuous)").getOrElse(null)
          }
        }

        it("should reject other options") {
          assert(CommandParser(s"/create_poll ($name) (yes) (ok)").isEmpty)
        }

        it("should require the parentheses") {
          assert(CommandParser(s"/create_poll ($name) (no) afterstop").isEmpty)
        }

        it("shouldn't let to leave the previous argument") {
          assert(CommandParser(s"/create_poll ($name) (afterstop)").isEmpty)
        }
      }

      describe("when start argument is presented") {
        it("should recognize correct date in HH:mm:ss yy:MM:dd format") {
          assertResult(CreatePollCommand(name, Some(true), Some(Afterstop()), Some(946684800))) {
            CommandParser(s"/create_poll ($name) (yes) (afterstop) (00:00:00 00:01:01)").getOrElse(null)
          }
        }

        it("should reject incorrect date in HH:mm:ss yy:MM:dd format") {
          assert(CommandParser(s"/create_poll ($name) (yes) (afterstop) (00:00:00 00:00:00)").isEmpty)
        }

        it("should fail on incorrect format") {
          assert(CommandParser(s"/create_poll ($name) (yes) (afterstop) (00:00:00 00-01-01)").isEmpty)
        }

        it("shouldn't let to leave the previous argument") {
          assert(CommandParser(s"/create_poll ($name) (yes) (00:00:00 00:01:01)").isEmpty)
        }
      }

      describe("when end argument is presented") {
        it("should recognize correct date in HH:mm:ss yy:MM:dd format") {
          assertResult(CreatePollCommand(name, Some(true), Some(Afterstop()), Some(946684800), Some(946684800))) {
            CommandParser(s"/create_poll ($name) (yes) (afterstop) (00:00:00 00:01:01) (00:00:00 00:01:01)").getOrElse(null)
          }
        }

        it("should reject incorrect date in HH:mm:ss yy:MM:dd format") {
          assert(CommandParser(s"/create_poll ($name) (yes) (afterstop) (00:00:00 00:01:01) (00:00:00 00:00:00)").isEmpty)
        }

        it("should fail on incorrect format") {
          assert(CommandParser(s"/create_poll ($name) (yes) (afterstop) (00:00:00 00:01:01) (00:00:00 00-01-01)").isEmpty)
        }
      }
    }

    def noArgCommand(commandName: String, instance: Command) = {
      describe(s"on $commandName command") {
        it(s"should return a $instance") {
          assertResult(instance) {
            CommandParser(commandName).getOrElse(null)
          }
        }

        it("should reject the command when trailing characters are presented") {
          assert(CommandParser(s"${commandName}a").isEmpty)
        }
      }
    }

    noArgCommand("/list", ListCommand())
    noArgCommand("/view", ViewCommand())
    noArgCommand("/end", EndCommand())

    def pollIdCommand(commandName: String, instance: Command) = {
      describe(s"on $commandName command") {
        it("should require id") {
          assert(CommandParser(commandName).isEmpty)
        }

        describe("when poll_id argument is presented") {
          it("should recognize id represented by digits") {
            assertResult(instance) {
              CommandParser(s"$commandName ($uid)").getOrElse(null)
            }
          }

          it("should reject id represented by negative numbers") {
            assert(CommandParser(s"$commandName (-$uid)").isEmpty)
          }

          it("should reject id represented by other characters") {
            assert(CommandParser(s"$commandName (asd)").isEmpty)
          }

          it ("should require the parentheses") {
            assert(CommandParser(s"$commandName $uid").isEmpty)
          }
        }
      }
    }

    pollIdCommand("/delete_poll", DeletePollCommand(uid))
    pollIdCommand("/start_poll", StartPollCommand(uid))
    pollIdCommand("/stop_poll", StopPollCommand(uid))
    pollIdCommand("/result", ResultCommand(uid))
    pollIdCommand("/begin", BeginCommand(uid))

    describe("on /add_question command") {
      val question = "whatsup?"

      it("should reqiure a question") {
        assert(CommandParser(s"/add_question").isEmpty)
      }

      describe("when question is presented") {
        it("should return AddQuestionCommand with \"open\" type") {
          assertResult(AddQuestionCommand(question, "open")) {
            CommandParser(s"/add_question ($question)").getOrElse(null)
          }
        }

        it ("should require the parentheses around the question") {
          assert(CommandParser(s"/add_question $question").isEmpty)
        }
      }

      describe("when type argument is presented") {
        it("should recognize open") {
          assertResult(AddQuestionCommand(question, "open")) {
            CommandParser(s"/add_question ($question) (open)").getOrElse(null)
          }
        }

        it("should require the parentheses") {
          assert(CommandParser(s"/add_question ($question) open").isEmpty)
        }

        describe("when type is choice") {
          it("should require at least an option") {
            assert(CommandParser(s"/add_question ($question) (choice)").isEmpty)
          }

          it("should recognize the options as a list of strings") {
            assertResult(AddQuestionCommand(question, "choice", List("op1", "op2"))) {
              CommandParser(s"/add_question ($question) (choice)\nop1\nop2").getOrElse(null)
            }
          }
        }

        describe("when type is multi") {
          it("should require at least an option") {
            assert(CommandParser(s"/add_question ($question) (multi)").isEmpty)
          }

          it("should recognize the options as a list of strings") {
            assertResult(AddQuestionCommand(question, "multi", List("op1", "op2"))) {
              CommandParser(s"/add_question ($question) (multi)\nop1\nop2").getOrElse(null)
            }
          }
        }

        it("should reject other options") {
          assert(CommandParser(s"/add_question ($question) (ok)").isEmpty)
        }
      }
    }

    describe("on /delete_question command ") {
      it("should require a question number") {
        assert(CommandParser("/delete_question").isEmpty)
      }

      describe("when question number is presented") {
        it("should recognize positive number") {
          assertResult(DeleteQuestionCommand(n)) {
            CommandParser(s"/delete_question ($n)").getOrElse(null)
          }
        }

        it("should require the parentheses") {
          assert(CommandParser(s"/delete_question $n").isEmpty)
        }
      }
    }

    describe("on /answer command") {
      val open = "Sure"

      it("should require a question number") {
        assert(CommandParser("/answer").isEmpty)
      }

      describe("when question number is presented") {
        it("should require an answer") {
          assert(CommandParser(s"/answer ($n)").isEmpty)
        }

        describe("when answer is presented") {
          it("should recognize an answer as an arbitary string") {
            assertResult(AnswerCommand(n, open)) {
              CommandParser(s"/answer ($n) ($open)").getOrElse(null)
            }
          }

          it("should require the parentheses around a question number") {
            assert(CommandParser(s"/answer $n ($open)").isEmpty)
          }

          it("should require the parentheses around an answer") {
            assert(CommandParser(s"/answer ($n) $open").isEmpty)
          }
        }
      }
    }
  }
}