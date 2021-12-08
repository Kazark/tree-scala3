package metalepsis

import cats.*
import cats.arrow.*

final case class Empty[A]()

object Empty:
  given [A]: Empty[A] = Empty()

  def toOptionK: Empty ~> Option =
    new FunctionK[Empty, Option]:
      override def apply[A](x: Empty[A]) = None
