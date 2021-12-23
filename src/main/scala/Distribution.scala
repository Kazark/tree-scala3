package metalepsis

import cats.*
import cats.syntax.all.*

sealed trait Distribution[A]

object Distribution:
  private case class Impl[A](
    events: List[(A, Double)]
  ) extends Distribution[A]

  given Monad[Distribution] with
    override def map[A, B](
      fa: Distribution[A]
    )(
      f: A => B
    ): Distribution[B] =
      fa match
        case Impl(events) =>
          Impl(events.map{case (k, v) => f(k) -> v})

    override def flatMap[A, B](
      fa: Distribution[A]
    )(
      f: A => Distribution[B]
    ): Distribution[B] =
      fa match
        case Impl(events) =>
          Impl(
            events.flatMap { case (k, v) =>
              f(k) match
                case Impl(pos_) => pos_.map(_.map(_ * v))
            }
          )

    override def pure[A](x: A): Distribution[A] =
      uniform(List(x))

    override def tailRecM[A, B](
      a: A
    )(
      f: A => Distribution[Either[A, B]]
    ): Distribution[B] =
      def g(x: A, p: Double): List[Either[(A, Double), (B, Double)]] =
        f(x) match
          case Impl(events) => events.map { case (k, v) =>
            k.bimap((_, v * p), (_, v * p))
          }
      Impl(Monad[List].tailRecM((a, 1.0))(g))

  def uniform[A](l: List[A]): Distribution[A] =
    val pb = 1.0/l.length
    Impl(l.map(x => (x -> pb)))

  given [A: Eq]: Eq[Distribution[A]] =
    Eq.by { case Impl(events) => events }
