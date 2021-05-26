package metalepsis

trait Database:
  def get(key: String): String

object Database:
  trait Get[E]:
    def db(e: E): Database

  def get[E](using get: Get[E]): DI[E, String => String] =
    DI(env => get.db(env).get)
