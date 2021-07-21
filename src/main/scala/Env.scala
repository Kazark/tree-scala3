package metalepsis

opaque type Env = ((Database, UI))

object Env:
  def of(
    db: Database,
    ui: UI
  ): Env = (db, ui)

  given uiGetEnv: UI.Get[Env] with
    def ui(e: Env): UI = e._2

  given dbGetEnv: Database.Get[Env] with
    def db(e: Env): Database = e._1
