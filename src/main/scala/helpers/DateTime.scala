package botopros.helpers

import java.time.{LocalDateTime, ZoneId, ZoneOffset}
import java.time.format.{DateTimeFormatter, DateTimeParseException}

object DateTime {
  val formatString = "HH:mm:ss yy:MM:dd"
  val zone = ZoneId.of("UTC")
  val formatter = DateTimeFormatter.ofPattern(formatString)

  def now: Long = LocalDateTime.now(zone).toEpochSecond(ZoneOffset.UTC)

  def parse(str: String): Option[Long] = {
    try {
      val date = LocalDateTime.parse(str, formatter)
      Some(date.toEpochSecond(ZoneOffset.UTC))
    } catch {
      case e: DateTimeParseException => None
    }
  }
}