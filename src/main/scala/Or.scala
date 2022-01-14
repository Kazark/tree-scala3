package church

sealed trait Or[A, B] {
  def fold[C](ifLeft: A => C, ifRight: B => C): C
}

object Or {
  // apply
  def left[A, B](x: A): Or[A, B] =
    new Or[A, B] {
      override def fold[C](ifLeft: A => C, ifRight: B => C): C =
        ifLeft(x)
    }

  def right[A, B](x: B): Or[A, B] =
    new Or[A, B] {
      override def fold[C](ifLeft: A => C, ifRight: B => C): C =
        ifRight(x)
    }
}
