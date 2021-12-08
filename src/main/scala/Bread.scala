package metalepsis

enum Bread:
  case TexasToast
  case Rye
  case Sourdough

object Bread:
  given Label[Bread] = Label("bread")
  given Options[Bread] = Options(values.map(_.toString).toList)
  given Price[Bread] = Price.included
