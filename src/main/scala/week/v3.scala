package week

import cats._
import java.time._
import scala.concurrent.duration._

object v3:
  // Here we have extracted a type-level variable in this datatype, decreasing
  // duplication, and increasing normalization.
  case class Week[A](
      sunday: A,
      monday: A,
      tuesday: A,
      wednesday: A,
      thursday: A,
      friday: A,
      saturday: A,
  ):
    def get(day: DayOfWeek): A =
      day match
        case DayOfWeek.SUNDAY => sunday
        case DayOfWeek.MONDAY => monday
        case DayOfWeek.TUESDAY => tuesday
        case DayOfWeek.WEDNESDAY => wednesday
        case DayOfWeek.THURSDAY => thursday
        case DayOfWeek.FRIDAY => friday
        case DayOfWeek.SATURDAY => saturday

    def set(day: DayOfWeek)(h: A): Week[A] =
      day match
        case DayOfWeek.SUNDAY => copy(sunday = h)
        case DayOfWeek.MONDAY => copy(monday = h)
        case DayOfWeek.TUESDAY => copy(tuesday = h)
        case DayOfWeek.WEDNESDAY => copy(wednesday = h)
        case DayOfWeek.THURSDAY => copy(thursday = h)
        case DayOfWeek.FRIDAY => copy(friday = h)
        case DayOfWeek.SATURDAY => copy(saturday = h)

  object Week:
    def byWorkDayVersusWeekend[A](workday: A, weekend: A): Week[A] =
      Week(
        sunday = weekend,
        monday = workday,
        tuesday = workday,
        wednesday = workday,
        thursday = workday,
        friday = workday,
        saturday = weekend,
      )

    def uniform[A](h: A): Week[A] =
      byWorkDayVersusWeekend(h, h)

  type WeeklyHours = Week[Hours]

  // But that only solved half our problem, because to write this:
  // def hoursOpenPerDay(w: Week[Hours]): Week[FiniteDuration] = ???
  // we are still going to need to write boilerplate. And then, the next time we
  // touch the datatype, we will have boilerplate again. And so on.

  // Let's write the boilerplate canonically, exactly once.
  // (Aside: this is so canonical that we could just get a macro or the compiler
  // to do it for us---an example of this is Kittens.)

  implicit val functor: Functor[Week] = ???
