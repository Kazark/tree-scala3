package metalepsis

import DI._

val justSendIt: DI[Env, String => Unit] =
  Database.get[Env].andThen(UI.showText)

val piersPlowman: DI[Env, Int] =
  justSendIt.map(sendForKey => sendForKey("sonne"))
    .and(DI.const(42))
