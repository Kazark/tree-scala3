package metalepsis

opaque type Env = ((Database, Bus))

object Env:
  def of(
    db: Database,
    bus: Bus
  ): Env = (db, bus)

  given busGetEnv: Bus.Get[Env] with
    def bus(e: Env): Bus = e._2

  given dbGetEnv: Database.Get[Env] with
    def db(e: Env): Database = e._1
