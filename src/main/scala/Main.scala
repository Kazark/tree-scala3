package metalepsis

class InMemDb(kv: Map[String, String]) extends Database {
  override def get(key: String): String =
    kv.get(key).getOrElse("and the")
}

object ConsoleBus extends Bus {
  def publish(kv: String) =
    print(s"$kv ")
}

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
  val x = sillyProgram.run(Env.of(db, ConsoleBus))
  print("\n--\n")
  x
