package metalepsis

import cats.*
import cats.syntax.all.*

// Identity Functor
// apply : A => Id A
// unapply : Id A => A // _.value

final case class Id[A](value: A)

// Cardinality = 2
// Bar = Boolean
// apply1 : Bar
// apply2 : Bar
// unapply : A => A => Bar => A

sealed trait Bool
object Bool {
  private object False extends Bool
  private object True extends Bool

  def `false`: Bool = False // apply 1
  def `true`: Bool = True // apply 1

  def ifThen[A](thenValue: A, elseValue: A, condition: Bool): A =
    condition match {
      case True => thenValue
      case False => elseValue
    }
}

// Pair - product
// apply : A => B => (A, B)
// unapply1 : (A, B) => A // _.fst
// unapply2 : (A, B) => B // _.snd

final case class Pair[A, B](fst: A, snd: B)

// // Either - coproduct
// apply1 : A => WhatTypeAmI2[A, B] // Left
// apply1 : B => WhatTypeAmI2[A, B] // Right
// unapply : (A => C) => (B => C) => WhatTypeAmI2[A, B] => C // pattern match / fold

sealed trait Either[A, B]

object Either {
  private final case class Left[A, B](value: A) extends Either[A, B]
  private final case class Right[A, B](value: B) extends Either[A, B]

  def left[A, B](value: A): Either[A, B] = Left(value)
  def right[A, B](value: B): Either[A, B] = Right(value)

  def fold[A, B, C](f: A => C, g: B => C, x: Either[A, B]): C =
    x match {
      case Left(value) => f(value)
      case Right(value) => g(value)
    }
}

// apply : (R => A) => Reader[R, A]
// unapply : Reader[R, A] => R => A
final case class Reader[R, A](run: R => A)

object Reader {
  implicit def monad[R]: Applicative[[X] =>> Reader[R, X]] =
    new Applicative[[X] =>> Reader[R, X]] {
      override def map[A, B](fa: Reader[R, A])(f: A => B): Reader[R, B] =
        Reader(fa.run.andThen(f))

      override def ap[A, B](ff: Reader[R, A => B])(fa: Reader[R, A]): Reader[R, B] =
        Reader(r => ff.run(r).apply(fa.run(r)))

      override def pure[A](x: A): Reader[R, A] =
        Reader(_ => x)

      def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
        Reader(r => f(fa.run(r)).run(r))
      }
    }
}

final case class ReaderT[F[_], R, A](run: R => F[A])

object ReaderT {
  implicit def monad[F[_]: Monad, R]: Applicative[[X] =>> ReaderT[F, R, X]] =
    new Applicative[[X] =>> ReaderT[F, R, X]] {
      override def map[A, B](fa: ReaderT[F, R, A])(f: A => B): ReaderT[F, R, B] =
        ReaderT(fa.run.andThen(_.map(f)))

      override def ap[A, B](ff: ReaderT[F, R, A => B])(fa: ReaderT[F, R, A]): ReaderT[F, R, B] =
        ReaderT(r => ff.run(r).ap(fa.run(r)))

      override def pure[A](x: A): ReaderT[F, R, A] =
        ReaderT(_ => x.pure)

      def flatMap[A, B](fa: ReaderT[F, R, A])(f: A => ReaderT[F, R, B]): ReaderT[F, R, B] = {
        ReaderT(r => fa.run(r).flatMap(f(_).run(r)))
      }
    }
}

@main def main(): Unit =
  println("Hello, world!")
