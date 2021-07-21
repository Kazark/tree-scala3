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

class InA(bus: Bus) {
  def talk(): Unit =
    bus.publish("In")
    bus.publish("a")
}

class SoftWasThe(bus: Bus) {
  def talk(): Unit =
    bus.publish("soft")
    bus.publish("was")
    bus.publish("the")
}

class Const[A](env: A) {
  val get: Int = 42
}

class PiersPlowman(db: Database, bus: Bus) {
  def execute(): Int =
    InA(bus).talk()
    GlomIt(db, bus).glomThese("somer", "seson", "whan")
    SoftWasThe(bus).talk()
    JustSendIt(db, bus).sendForKey("sonne") // <- check
    Const((db, bus)).get // <- check
}
