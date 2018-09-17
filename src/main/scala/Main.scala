package botopros

import scala.concurrent._
import scala.concurrent.duration._

object Main {
  def main(args: Array[String]): Unit = {
    val eol = BotOpros.run()
    println("Press [ENTER] to shutdown the bot, it may take a few seconds...")
    scala.io.StdIn.readLine()
    BotOpros.shutdown()
    Await.result(eol, Duration.Inf)
  }
}