package church

sealed trait Nat {
  def fold[A](ifSucc: A => A, ifZero: A): A
}

object Nat {
  val zero: Nat =
    new Nat {
      override def fold[A](ifSucc: A => A, ifZero: A): A =
        ifZero
    }

  val one: Nat = succ(zero)
  val two: Nat = succ(one)
  val three: Nat = succ(two)
  val four: Nat = succ(three)
  val five: Nat = succ(four)
  val six: Nat = succ(five)
  val seven: Nat = succ(six)
  val eight: Nat = succ(seven)
  val nine: Nat = succ(eight)

  def succ(n: Nat): Nat =
    new Nat {
      // recursive
      override def fold[A](ifSucc: A => A, ifZero: A): A =
        val n_ = n.fold(ifSucc, ifZero)
        val `n+1` = ifSucc(n_) // inductive step
        `n+1`
    }

  def toInt(n: Nat): Int =
    n.fold(_ + 1, 0)
}
