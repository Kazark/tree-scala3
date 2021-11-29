package metalepsis

trait Database:
  def get(key: String): String

object Database:
  trait Get[E]:
    def db(e: E): Database

  def get[E](using get: Get[E])(key: String): DI[E, String] =
    DI(env => get.db(env).get(key))
