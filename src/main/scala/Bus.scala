package metalepsis

trait Bus:
  def publish(msg: String): Unit

object Bus:
  trait Get[E]:
    def bus(e: E): Bus

  def publish[E](using get: Get[E]): DI[E, String => Unit] =
    DI(env => msg => get.bus(env).publish(msg))
