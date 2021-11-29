package metalepsis

import DI._

def justSendIt(key: String): DI[Env, Unit] =
  for
   value <- Database.get[Env](key)
   _ <- UI.showText[Env](value)
  yield ()

val softWasThe: DI[Env, Unit] =
  for
    _ <- UI.showText[Env]("soft")
    _ <- UI.showText[Env]("was")
    _ <- UI.showText[Env]("the")
  yield ()

def glomThese(key1: String, key2: String, key3: String): DI[Env, Unit] =
  for
    value1 <- Database.get[Env](key1)
    value2 <- Database.get[Env](key2)
    value3 <- Database.get[Env](key3)
    _ <- UI.showText[Env](s"$value1 $value2 $value3")
  yield ()

val inA: DI[Env, Unit] =
  for
    _ <- UI.showText[Env]("In")
    _ <- UI.showText[Env]("a")
  yield ()

val piersPlowman: DI[Env, Int] =
  for
    _ <- inA
    _ <- glomThese("somer", "seson", "whan")
    _ <- softWasThe
    _ <- justSendIt("sonne")
    x <- DI.const(42)
  yield x
