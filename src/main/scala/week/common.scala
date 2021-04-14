package week

import java.time._

enum Hours:
  case Closed
  case Open(from: LocalTime, to: LocalTime)

object Hours {
  def nineToSix: Hours =
    Open(
      LocalTime.MIDNIGHT.plus(Duration.ofHours(9)),
      LocalTime.NOON.plus(Duration.ofHours(6)),
    )
}
