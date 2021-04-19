package metalepsis

import cats._
import cats.syntax.all._

case class State[S, A](run: S => (S, A))

object State:
  // Read from a mutable variable
  def get[S]: State[S, S] =
    State(s => (s, s))

  // Write to a mutable variable
  def set[S](x: S): State[S, Unit] =
    State(_ => (x, ()))

  def modify[S](f: S => S): State[S, Unit] =
    State(s => (f(s), ()))

  implicit def monad[S]: Monad[[A] =>> State[S, A]] =
    new Monad[[A] =>> State[S, A]] {
      // IDENTITY: map(x)(identity) === x
      // COMPOSITION: map(map(x)(f))(g) === map(x)(f.andThen(g))
      override def map[A, B](fa: State[S, A])(f: A => B): State[S, B] =
        State { s =>
          val (s2, x) = fa.run(s)
          (s2, f(x))
        }

      override def pure[A](x: A): State[S, A] =
        ???

      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
        ???

      override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] =
        ???
    }

// Referential transparency

@main def main(): Unit =
  def x: State[Int, Int] =
    for {
      () <- State.modify((y: Int) => y + 3)
      x <- State.get
    } yield x
  val y: State[Int, Int] = x.map(qux => 2 + qux)
  val bar: State[Int, Int] = x.map(qux => 2 + qux)
  val z: State[Int, Int] = State(s => (s, 2 + 5 + 3))
  println(s"${y.run(5)} =?= ${bar.run(5)}")
  println(s"${y.run(5)} =?= ${z.run(5)}")
