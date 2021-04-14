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
    JustSendIt(db, bus).sendForKey("foo")
    GlomIt(db, bus).glomThese("bar", "baz", "qux")
    YadaYadaYada(bus).talk()
    Const((db, bus)).get
}

@main def main(): Int =
  val db = InMemDb(
    Map(
      "foo" -> "Twas",
      "bar" -> "brillig",
      "qux" -> "slithey toves"
    )
  )
  SillyProgram(db, ConsoleBus).execute()
