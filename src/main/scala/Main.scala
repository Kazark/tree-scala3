package metalepsis

import cats.*
import cats.syntax.all.*
import scala.io.StdIn

def select(selection: String): Option[Offering[Empty]] =
  selection match
    case Sandwich.Tag => Some(Sandwich.summon[Empty])
    case Pizza.Tag => Some(Pizza.summon[Empty])
    case IceCream.Tag => Some(IceCream.summon[Empty])
    case _ => None

def customize(
  offering: Offering[Option]
)(
  customization: List[String]
): Either[Offering[Id], Offering[Option]] =
  customization match
    case List("done") =>
      val maybe =
        offering.fold(
          Sandwich.done,
          Pizza.done,
          IceCream.done,
        )
      maybe.toLeft(offering)
    case _ =>
      val customize = offering.fold(
        Sandwich.customize,
        Pizza.customize,
        IceCream.customize,
      )
      val maybe = PartialFunction.condOpt(customization)(customize)
      Right(maybe.getOrElse(offering))

def price(offering: Offering[Id]): Money =
  Foldable[List].fold(
    offering.cancelWith(
      Sandwich.summon[Price],
      Pizza.summon[Price],
      IceCream.summon[Price]
    )
  )

def prompt(): String =
  print("> ")
  StdIn.readLine()

@main def main(): Unit =
  var maybe: Option[Offering[Empty]] = None
  while (maybe.isEmpty) {
    println(Offering.prompt.mkString("\n"))
    val selection = prompt()
    maybe = select(selection)
    if (maybe.isEmpty) {
      println(s"? $selection")
    }
  }
  var offering: Offering[Option] = maybe.get.mapK(Empty.toOptionK)
  var customized: Offering[Id] = null
  while (customized == null) {
    val customization = prompt()
    customize(offering)(customization.split(' ').toList) match
      case Left(c) => customized = c
      case Right(o) => offering = o
  }
  println(s"You selected: ${customized}")
  println(s"You owe: ${price(customized)}")
