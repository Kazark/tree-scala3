package metalepsis

trait Database {
  def get(key: String): String
}

trait Bus {
  def publish(msg: String): Unit
}

class JustSendIt(db: Database, bus: Bus) {
  def sendForKey(key: String): Unit = {
    val value = db.get(key)
    bus.publish(value)
  }

  def sendForKey2(key: Int, different: Long): Unit = {
    ???
  }
}

class GlomIt(db: Database, bus: Bus) {
  def glomThese(key1: String, key2: String, key3: String): Unit = {
    val value1 = db.get(key1)
    val value2 = db.get(key2)
    val value3 = db.get(key3)
    bus.publish(s"$value1$value2$value3")
  }
}

class YadaYadaYada(bus: Bus) {
  def talk(): Int = {
    bus.publish("yada yada yada!")
    5
  }
}

class Const[A](env: A) {
  val get: Int = 42
}

class DepInj[A, B, C](env: A) {
  def execute(args: B): C =
    ???
}

object DepInj {
  type JustSendIt = DepInj[(Database, Bus), String, Unit]
  type GlomIt = DepInj[(Database, Bus), (String, String, String), Unit]
  type YadaYadaYada = DepInj[Bus, Unit, Int]
  //type DI[A, B, C] = A => B => C
  type DI[A, B] = A => B
  type DI3[A, B, C] = DI[A, B => C]
  //type DI[A, B, C] = Function[A, Function[B, C]]

  def add(x: Int, y: Int): Int =
    x + y

  def add(x: Int): Int => Int = y => x + y
}
