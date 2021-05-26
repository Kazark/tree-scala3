package metalepsis

case class DI[E, A](run: E => A) {
  // contramap
  def local[E2](f: E2 => E): DI[E2, A] =
    DI(f.andThen(run))

  def map[B](f: A => B): DI[E, B] =
    DI(run.andThen(f))
}

object DI {
  type DI3[A, B, C] = DI[A, B => C]

  def const[E, A](x: A): DI[E, A] =
    DI(_ => x)

  def andThen[E, A, B, C](
    diF: DI[E, A => B],
    diG: DI[E, B => C],
  ): DI[E, A => C] =
    // DI { env => diF.run(env).andThen(diG.run(env)) }
    DI { env =>
      val f: A => B = diF.run(env)
      val g: B => C = diG.run(env)
      x => g(f(x))
    }

  // DI3[(Database, Bus), String, Unit]
  def combine[E, A, B](
    diA: DI[E, A],
    diB: DI[E, B],
  ): DI[E, B] =
    DI { env =>
      val _: A = diA.run(env)
      val b: B = diB.run(env)
      b
    }
}


object DBAndBus {
  def bus: ((Database, Bus)) => Bus = x => x._2
  def db: ((Database, Bus)) => Database = x => x._1
}

object Example {
  import DI._

  val justSendIt: DI3[(Database, Bus), String, Unit] =
    andThen(
      Database.get.local(DBAndBus.db),
      Bus.publish.local(DBAndBus.bus)
    )

  // val justSendIt: DI3[(Database, Bus), String, Unit] = DI {
  //   (dbAndBus: (Database, Bus)) =>
  //     (key: String) => {
  //       val (db, bus) = dbAndBus
  //       val value = db.get(key)
  //       bus.publish(value)
  //     }
  // }
}
