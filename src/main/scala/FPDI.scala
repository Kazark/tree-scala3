package metalepsis

import DI._

val justSendIt: DI[Env, String => Unit] =
  Database.get[Env].andThen(Bus.publish)

val sillyProgram: DI[Env, Int] =
  justSendIt.map(sendForKey => sendForKey("foo"))
    .and(DI.const(42))
