package metalepsis

case class DI[E, A](run: E => A)

object DI {
  type DI3[A, B, C] = DI[A, B => C]

  // public static <E, A> DI<E, A> const(A x) {
  //   return new DI(_ -> x);
  // }
  def const[E, A](x: A): DI[E, A] =
    DI(_ => x)

  val justSendIt: DI3[(Database, Bus), String, Unit] = DI {
    (dbAndBus: (Database, Bus)) =>
      (key: String) => {
        val (db, bus) = dbAndBus
        val value = db.get(key)
        bus.publish(value)
      }
  }
}
