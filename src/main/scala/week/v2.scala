package week

import java.time._
import scala.concurrent.duration._

object v2:
  case class WeeklyHours(
      sunday: Hours,
      monday: Hours,
      tuesday: Hours,
      wednesday: Hours,
      thursday: Hours,
      friday: Hours,
      saturday: Hours,
  ):
    def get(day: DayOfWeek): Hours =
      day match
        case DayOfWeek.SUNDAY => sunday
        case DayOfWeek.MONDAY => monday
        case DayOfWeek.TUESDAY => tuesday
        case DayOfWeek.WEDNESDAY => wednesday
        case DayOfWeek.THURSDAY => thursday
        case DayOfWeek.FRIDAY => friday
        case DayOfWeek.SATURDAY => saturday

    def set(day: DayOfWeek)(h: Hours): WeeklyHours =
      day match
        case DayOfWeek.SUNDAY => copy(sunday = h)
        case DayOfWeek.MONDAY => copy(monday = h)
        case DayOfWeek.TUESDAY => copy(tuesday = h)
        case DayOfWeek.WEDNESDAY => copy(wednesday = h)
        case DayOfWeek.THURSDAY => copy(thursday = h)
        case DayOfWeek.FRIDAY => copy(friday = h)
        case DayOfWeek.SATURDAY => copy(saturday = h)

  object Weekly:
    def byWorkDayVersusWeekend(workday: Hours, weekend: Hours): WeeklyHours =
      WeeklyHours(
        sunday = weekend,
        monday = workday,
        tuesday = workday,
        wednesday = workday,
        thursday = workday,
        friday = workday,
        saturday = weekend,
      )

    def uniform(h: Hours): WeeklyHours =
      byWorkDayVersusWeekend(h, h)

    val default = byWorkDayVersusWeekend(
      Hours.nineToSix,
      Hours.Closed,
    )

  // But what if we wanted to calculate how many hours a bank was open each
  // day? Would we write a new type like this, and a lot of boilerplate to
  // translate to it? :grimace:
  case class WeeklyHours2(
      sunday: FiniteDuration,
      monday: FiniteDuration,
      tuesday: FiniteDuration,
      wednesday: FiniteDuration,
      thursday: FiniteDuration,
      friday: FiniteDuration,
      saturday: FiniteDuration,
  )

  def hoursOpenPerDay(w: WeeklyHours): WeeklyHours2 =
    ??? // Put 7x reptitive code here

  // Or what if we wanted to do something totally different, such as storing
  // statistics about the number of cases opened on average on each day of the
  // week? Would we again rebuild all of this?
  case class WeeklyStats(
      sunday: Integer,
      monday: Integer,
      tuesday: Integer,
      wednesday: Integer,
      thursday: Integer,
      friday: Integer,
      saturday: Integer,
  )

  // Observe that we have duplicated something concrete in our code, namely,
  // whichever type is associated with each day. What if we extracted the
  // duplication using a generic type instead?
