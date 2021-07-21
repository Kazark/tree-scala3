package metalepsis

import DI._

val justSendIt: DI[Env, String => Unit] =
  Database.get[Env].andThen(Bus.publish)

val piersPlowman: DI[Env, Int] =
  justSendIt.map(sendForKey => sendForKey("sonne"))
    .and(DI.const(42))
