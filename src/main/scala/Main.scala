package calc

enum Expr:
  case EInt(value: Int)
  case Add(leftAddend: Expr, rightAddend: Expr)
  case Minus(minuend: Expr, subtrahend: Expr)

def eval(expr: Expr): Int =
  expr match
    case Expr.Add(l, r) => eval(l) + eval(r)
    case Expr.Minus(m, s) => eval(m) - eval(s)
    case Expr.EInt(value) => value

@main def main(): Unit =
  val result = eval(Expr.Minus(minuend = Expr.EInt(256), subtrahend = Expr.Add(Expr.EInt(128), Expr.EInt(64))))
  println(result)
