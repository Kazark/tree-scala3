package metalepsis

trait CancelK[K[_[_]]]:
  def cancelK[F[_], A](
    f: K[[X] =>> F[X] => A],
    x: K[F],
  ): List[A]

extension [K[_[_]], F[_], A] (x: K[F]) def cancelK(
  f: K[[X] =>> F[X] => A]
)(using C: CancelK[K]): List[A] = C.cancelK(f, x)
