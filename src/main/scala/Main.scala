package metalepsis

import cats.*
import cats.syntax.all.*

final case class IO[A](run: () => A)

object IO {
  def putStrLn(value: String): IO[Unit] =
    IO(() => println(value))

  given Monad[IO] with
    def pure[A](x: A): IO[A] =
      IO(() => x)

    def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      IO { () =>
        var result: Option[B] = None
        var x = a
        while result.isEmpty do
          f(x).run() match
            case Left(a) => x = a
            case Right(b) => result = b.some
        result.get
      }

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      IO(() => f(fa.run()).run())
}

trait Ref[A] {
  def get: IO[A]
  def set(value: A): IO[Unit]
  def update(f: A => A): IO[A]
}

object Ref {
  def apply[A](init: A): IO[Ref[A]] =
    IO { () =>
      var x = init
      new Ref[A]:
        override def get =
          IO(() => x)
        override def set(value: A) =
          IO(() => x = value)
        override def update(f: A => A) =
          IO { () => x = f(x); x }
    }
}

val topLevel: IO[Unit] =
  for
    x <- Ref(0)
    incr = x.update(_ + 1) // Value
    x0 <- incr
    _ <- IO.putStrLn(x0.toString)
    x1 <- incr
    _ <- IO.putStrLn(x1.toString)
    x2 <- incr
    _ <- IO.putStrLn(x2.toString)
    x3 <- incr
    _ <- IO.putStrLn(x3.toString)
  yield ()

@main def main(): Unit =
  topLevel.run()
