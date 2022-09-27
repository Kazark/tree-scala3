package metalepsis

import cats.*
import scala.io.*
import scala.util.*

enum EffReq[A]:
  case Shuffle[B](xs: List[B]) extends EffReq[List[B]]
  case PrintLn(value: String) extends EffReq[Unit]
  case ReadLine extends EffReq[String]

sealed trait IO[A] {
  def bind[B](f: A => IO[B]): IO[B]
}

object IO:
  final case class Pure[A](value: A) extends IO[A]:
    override def bind[B](f: A => IO[B]): IO[B] = f(value)

  final case class Send[Z, A](req: EffReq[Z], cont: Z => A) extends IO[A]:
    override def bind[B](f: A => IO[B]): IO[B] =
      Flatten(() => Send[Z, IO[B]](req, cont.andThen(f)))

  final case class Flatten[A](inner: () => IO[IO[A]]) extends IO[A]:
    override def bind[B](f: A => IO[B]): IO[B] =
      inner().bind(_.bind(f))

  given Monad[IO] with
    override def pure[A](x: A): IO[A] = IO.Pure(x)

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      fa.bind(f)

    override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      ??? // TODO

  def unsafeRun[A](program: IO[A]): A =
    program match
      case Pure(x) => x
      case Send(EffReq.Shuffle(xs), cont) =>
        cont(Random.shuffle(xs))
      case Send(EffReq.PrintLn(value), cont) =>
        println(value)
        cont(())
      case Send(EffReq.ReadLine, cont) =>
        cont(StdIn.readLine())
      case Flatten(inner) =>
       unsafeRun(unsafeRun(inner()))

@main def main(): Unit =
  println("Hello, world!")
