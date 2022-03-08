package metalepsis

import cats.*
import cats.syntax.all.*

final case class Reader[R, A](run: R => A)

object Reader {
  implicit def monad[R]: Monad[[X] =>> Reader[R, X]] =
    new Monad[[X] =>> Reader[R, X]] {
      override def map[A, B](fa: Reader[R, A])(f: A => B): Reader[R, B] =
        Reader(fa.run.andThen(f))

      override def ap[A, B](ff: Reader[R, A => B])(fa: Reader[R, A]): Reader[R, B] =
        Reader(r => ff.run(r).apply(fa.run(r)))

      override def pure[A](x: A): Reader[R, A] =
        Reader(_ => x)

      override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(fa.run(r)).run(r))

      override def tailRecM[A, B](a: A)(f: A => Reader[R, Either[A, B]]): Reader[R, B] =
        ???
    }
}

final case class ReaderT[F[_], R, A](run: R => F[A])

object ReaderT {
  implicit def monad[F[_]: Monad, R]: Monad[[X] =>> ReaderT[F, R, X]] =
    new Monad[[X] =>> ReaderT[F, R, X]] {
      override def map[A, B](fa: ReaderT[F, R, A])(f: A => B): ReaderT[F, R, B] =
        ReaderT(fa.run.andThen(_.map(f)))

      override def ap[A, B](ff: ReaderT[F, R, A => B])(fa: ReaderT[F, R, A]): ReaderT[F, R, B] =
        ReaderT(r => ff.run(r).ap(fa.run(r)))

      override def pure[A](x: A): ReaderT[F, R, A] =
        ReaderT(_ => x.pure)

      override def flatMap[A, B](fa: ReaderT[F, R, A])(f: A => ReaderT[F, R, B]): ReaderT[F, R, B] = {
        ReaderT(r => fa.run(r).flatMap(f(_).run(r)))
      }

      override def tailRecM[A, B](a: A)(f: A => ReaderT[F, R, Either[A, B]]): ReaderT[F, R, B] =
        ???
    }
}

@main def main(): Unit =
  println("Hello, world!")
