package metalepsis

class InMemDb(kv: Map[String, String]) extends Database {
  override def get(key: String): String =
    kv.get(key).getOrElse("and the")
}

object ConsoleBus extends Bus {
  def publish(kv: String) =
    print(s"$kv ")
}

class SillyProgram(db: Database, bus: Bus) {
  def execute(): Int =
    JustSendIt(db, bus).sendForKey("foo") // <- check
    GlomIt(db, bus).glomThese("bar", "baz", "qux")
    YadaYadaYada(bus).talk()
    Const((db, bus)).get // <- check
}

val sillyProgram: DI[(Database, Bus), Int] =
  DI.combine(
    Example.justSendIt.map(sendForKey => sendForKey("foo")),
    DI.const(42)
  )

@main def main(): Int =
  val db = InMemDb(
    Map(
      "foo" -> "Twas",
      "bar" -> "brillig",
      "qux" -> "slithey toves"
    )
  )
  print("\n--\n")
  val _ = SillyProgram(db, ConsoleBus).execute()
  print("\n--\n")
  val x = sillyProgram.run((db, ConsoleBus))
  print("\n--\n")
  x
