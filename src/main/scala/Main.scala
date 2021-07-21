package metalepsis

class InMemDb(kv: Map[String, String]) extends Database:
  override def get(key: String): String =
    kv.get(key).get

object ConsoleUI extends UI:
  def showText(kv: String) =
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
  val _ = PiersPlowman(db, ConsoleUI).execute()
  print("\n--\n")
  val x = piersPlowman.run(Env.of(db, ConsoleUI))
  print("\n--\n")
  x
