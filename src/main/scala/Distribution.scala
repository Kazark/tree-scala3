package metalepsis

import cats.*
import cats.syntax.all.*

sealed trait Distribution[A]

object Distribution:
  private case class Impl[A](
    pos: List[(A, Double)]
  ) extends Distribution[A]

  given Monad[Distribution] with
    override def map[A, B](
      fa: Distribution[A]
    )(
      f: A => B
    ): Distribution[B] =
      fa match
        case Impl(pos) =>
          Impl(pos.map{case (k, v) => f(k) -> v})

    override def flatMap[A, B](
      fa: Distribution[A]
    )(
      f: A => Distribution[B]
    ): Distribution[B] =
      fa match
        case Impl(pos) =>
          val x = pos.flatMap { case (k, v) =>
            f(k) match
              case Impl(pos_) => pos_.map(_._1)
          }
          uniform(x)

    override def pure[A](x: A): Distribution[A] =
      uniform(List(x))

    override def tailRecM[A, B](
      a: A
    )(
      f: A => Distribution[Either[A, B]]
    ): Distribution[B] =
      val g: A => List[Either[A, B]] = f.andThen {
        case Impl(pos) => pos.map(_._1)
      }
      uniform(Monad[List].tailRecM(a)(g))

  def uniform[A](l: List[A]): Distribution[A] =
    val pb = 100.0/l.length
    Impl(l.map(x => (x -> pb)))

  given [A: Eq]: Eq[Distribution[A]] =
    Eq.by { case Impl(pos) => pos }
