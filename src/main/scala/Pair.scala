package church

sealed trait Pair[A, B]

object Pair {
  private abstract class Impl[A, B]() extends Pair[A, B] {
    def select[R](selector: A => B => R): R
  }

  def apply[A, B](x: A, y: B): Pair[A, B] =
    new Impl[A, B]() {
      override def select[R](selector: A => B => R): R =
        selector(x)(y)
    }

  def first[A, B](x: Pair[A, B]): A =
    x match {
      case impl: Impl[A, B] => impl.select(x => _ => x)
    }

  def second[A, B](x: Pair[A, B]): B =
    x match {
      case impl: Impl[A, B] => impl.select(_ => y => y)
    }

  def swap[A, B](x: Pair[A, B]): Pair[B, A] =
    x match {
      case impl: Impl[A, B] => impl.select(x => y => apply(y, x))
    }

  def toTuple2[A, B](x: Pair[A, B]): (A, B) =
    x match {
      case impl: Impl[A, B] =>
        impl.select(x => y => (x, y))
    }
}
