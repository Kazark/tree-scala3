package metalepsis

object V1: // OOP
  enum Either[A, B]:
    case Left(value: A)
    case Right(value: B)

  def fold[A, B, C](either: Either[A, B])(ifLeft: A => C, ifRight: B => C): C =
    either match
      case Either.Left(x) => ifLeft(x)
      case Either.Right(x) => ifRight(x)

object V2: // Church
  sealed trait Either[A, B]:
    def fold[C](ifLeft: A => C, ifRight: B => C): C

  object Either:
    def left[A, B](x: A): Either[A, B] =
      new Either[A, B]:
        override def fold[C](ifLeft: A => C, ifRight: B => C): C =
          ifLeft(x)
    def right[A, B](x: B): Either[A, B] =
      new Either[A, B]:
        override def fold[C](ifLeft: A => C, ifRight: B => C): C =
          ifRight(x)

object V3: // bool-pair
  sealed trait Either[A, B]:
    def fold[C](ifLeft: A => C, ifRight: B => C): C

  object Either:
    private final case class Impl[A, B](
      isRight: Boolean,
      value: Any,
    ) extends Either[A, B]:
      override def fold[C](ifLeft: A => C, ifRight: B => C): C =
        if isRight
        then ifRight(value.asInstanceOf[B])
        else ifLeft(value.asInstanceOf[A])

    def left[A, B](x: A): Either[A, B] =
      Impl(false, x)
    def right[A, B](x: A): Either[A, B] =
      Impl(true, x)

@main def main(): Unit =
  println("Hello, world!")
