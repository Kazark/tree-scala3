package metalepsis

trait Database {
  def get(key: String): String
}

object Database {
  val get: DI[Database, String => String] =
    DI(db => key => db.get(key))
}
