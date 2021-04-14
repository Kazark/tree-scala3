package week

object v0:
  type WeeklyHours = List[Hours]

  val canHaveTooFewDays: WeeklyHours = List()

  val canHaveTooManyDays: WeeklyHours =
    List(
      Hours.Closed, // 1
      Hours.Closed, // 2
      Hours.Closed, // 3
      Hours.Closed, // 4
      Hours.Closed, // 5
      Hours.Closed, // 6
      Hours.Closed, // 7
      Hours.Closed, // 8
    )

  val validWeek =
    List(
      Hours.Closed, // 1
      Hours.Closed, // 2
      Hours.Closed, // 3
      Hours.Closed, // 4
      Hours.Closed, // 5
      Hours.Closed, // 6
      Hours.Closed, // 7
    )

  val easyToScrewUp: Hours = validWeek(8) // BOOM!
