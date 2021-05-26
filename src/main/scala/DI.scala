package metalepsis

case class DI[E, A](run: E => A) {
  def local[E2](f: E2 => E): DI[E2, A] =
    DI(f.andThen(run))
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
}

object Example {
  import DI._

  val dbGet: DI3[Database, String, String] =
    DI(db => key => db.get(key))

  val dbGet2: DI[(Database, Bus), String => String] =
    dbGet.local(dbAndBus => dbAndBus._1)

  val dbPublish: DI3[Bus, String, Unit] =
    DI(bus => msg => bus.publish(msg))

  val dbPublish2: DI[(Database, Bus), String => Unit] =
    dbPublish.local(dbAndBus => dbAndBus._2)

  val justSendIt2: DI3[(Database, Bus), String, Unit] =
    andThen(dbGet2, dbPublish2)

  val justSendIt: DI3[(Database, Bus), String, Unit] = DI {
    (dbAndBus: (Database, Bus)) =>
      (key: String) => {
        val (db, bus) = dbAndBus
        val value = db.get(key)
        bus.publish(value)
      }
  }
}
