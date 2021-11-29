package metalepsis

case class DI[E, A](run: E => A):
  def local[E2](f: E2 => E): DI[E2, A] =
    DI(f.andThen(run))

  def map[B](f: A => B): DI[E, B] =
    DI(run.andThen(f))

  def flatMap[B](f: A => DI[E, B]): DI[E, B] =
    DI { env =>
      val x = this.run(env)
      val that = f(x)
      that.run(env)
    }

  def and[B](that: DI[E, B]): DI[E, B] =
    flatMap(_ => that)

object DI:
  def const[E, A](x: A): DI[E, A] =
    DI(_ => x)

extension [E, A, B, C](diF: DI[E, A => B])
  def andThen(diG: DI[E, B => C]): DI[E, A => C] =
    DI(env => diF.run(env).andThen(diG.run(env)))
