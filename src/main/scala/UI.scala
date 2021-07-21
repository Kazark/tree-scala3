package metalepsis

trait UI:
  def showText(msg: String): Unit

object UI:
  trait Get[E]:
    def ui(e: E): UI

  def showText[E](using get: Get[E]): DI[E, String => Unit] =
    DI(env => msg => get.ui(env).showText(msg))
