package metalepsis

class JustSendIt(db: Database, ui: UI) {
  def sendForKey(key: String): Unit = {
    val value = db.get(key)
    ui.showText(value)
  }
}

class GlomIt(db: Database, ui: UI) {
  def glomThese(key1: String, key2: String, key3: String): Unit = {
    val value1 = db.get(key1)
    val value2 = db.get(key2)
    val value3 = db.get(key3)
    ui.showText(s"$value1 $value2 $value3")
  }
}

class InA(ui: UI) {
  def talk(): Unit =
    ui.showText("In")
    ui.showText("a")
}

class SoftWasThe(ui: UI) {
  def talk(): Unit =
    ui.showText("soft")
    ui.showText("was")
    ui.showText("the")
}

class Const[A](env: A) {
  val get: Int = 42
}

class PiersPlowman(db: Database, ui: UI) {
  def execute(): Int =
    InA(ui).talk()
    GlomIt(db, ui).glomThese("somer", "seson", "whan")
    SoftWasThe(ui).talk()
    JustSendIt(db, ui).sendForKey("sonne") // <- check
    Const((db, ui)).get // <- check
}
