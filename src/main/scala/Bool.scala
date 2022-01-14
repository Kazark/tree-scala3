package church

sealed trait Bool {
  def cond[A](thenValue: A, elseValue: A): A
}

object Bool {
  object False extends Bool {
    override def cond[A](thenValue: A, elseValue: A): A =
      elseValue
  }

  object True extends Bool {
    override def cond[A](thenValue: A, elseValue: A): A =
      thenValue
  }
}
