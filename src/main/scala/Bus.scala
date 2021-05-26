package metalepsis

trait Bus {
  def publish(msg: String): Unit
}

object Bus {
  val publish: DI[Bus, String => Unit] =
    DI(bus => msg => bus.publish(msg))
}
