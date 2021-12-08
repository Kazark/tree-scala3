package metalepsis

import cats.*

// terrible way to model currency, of course
final case class Money(value: Double)

object Money {
  given Monoid[Money] with
    val empty = Money(0.0)
    def combine(x: Money, y: Money) = Money(x.value + y.value)
}

type Price[A] = Function[A, Money]

object Price:
  def included[A]: Price[A] = _ => Monoid[Money].empty
