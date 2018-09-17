package botopros

import com.bot4s.telegram.api.{TelegramBot, Polling}
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.methods.{SendMessage, ParseMode}
import com.bot4s.telegram.models.Message

import botopros.handlers.{Dispatcher, Messages}
import botopros.helpers.State
import botopros.models._

object BotOpros extends TelegramBot with Polling {
  val token = sys.env.get("BOT_TOKEN").get
  val client = new ScalajHttpClient(token)

  var stateData = StateData(Map(), Map(), 0)

  override def receiveMessage(msg: Message): Unit = {
    val botState: BotState = msg.from match {
      case None => State.unit(Messages.noUser)
      case Some(u) => msg.text match {
        case None => State.unit(Messages.noCommand)
        case Some(t) => Dispatcher.handle(t, User(u.id, u.firstName, u.lastName))
      }
    }
    val (text, nextStateData) = botState.run(stateData)
    stateData = nextStateData
    request(SendMessage(msg.source, text, Some(ParseMode.HTML)))
  }
}