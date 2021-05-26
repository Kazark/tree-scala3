package metalepsis

class JustSendIt(db: Database, bus: Bus) {
  def sendForKey(key: String): Unit = {
    val value = db.get(key)
    bus.publish(value)
  }
}

class GlomIt(db: Database, bus: Bus) {
  def glomThese(key1: String, key2: String, key3: String): Unit = {
    val value1 = db.get(key1)
    val value2 = db.get(key2)
    val value3 = db.get(key3)
    bus.publish(s"$value1 $value2 $value3")
  }
}

class YadaYadaYada(bus: Bus) {
  def talk(): Unit =
    bus.publish("did gyre and gimble in the wabe")
}

class Const[A](env: A) {
  val get: Int = 42
}

class SillyProgram(db: Database, bus: Bus) {
  def execute(): Int =
    JustSendIt(db, bus).sendForKey("foo") // <- check
    GlomIt(db, bus).glomThese("bar", "baz", "qux")
    YadaYadaYada(bus).talk()
    Const((db, bus)).get // <- check
}
