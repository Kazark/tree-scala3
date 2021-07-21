package metalepsis

class InMemDb(kv: Map[String, String]) extends Database:
  override def get(key: String): String =
    kv.get(key).get

object ConsoleBus extends Bus:
  def publish(kv: String) =
    print(s"$kv ")

@main def main(): Int =
  val db = InMemDb(
    Map(
      // Translations of Middle English words
      "somer" -> "summer",
      "seson" -> "season",
      "whan" -> "when",
      "sonne" -> "sun",
    )
  )
  print("\n--\n")
  val _ = PiersPlowman(db, ConsoleBus).execute()
  print("\n--\n")
  val x = piersPlowman.run(Env.of(db, ConsoleBus))
  print("\n--\n")
  x
