package metalepsis

import cats.*
import scala.io.StdIn

def select(selection: String): Option[Offering[Empty]] =
  selection match
    case "sandwich" => Some(Sandwich.summon[Empty])
    case "pizza" => Some(Pizza.summon[Empty])
    case "ice-cream" => Some(IceCream.summon[Empty])
    case _ => None

def customize(
  offering: Offering[Option]
)(
  customization: String
): Either[Offering[Id], Offering[Option]] =
  offering match
    case o: IceCream[Option] => IceCream.customize(o)(customization)
    case _ => ???

def price(offering: Offering[Id]): Money =
  Foldable[List].fold(
    offering.cancelWith(
      Sandwich.summon[Price],
      Pizza.summon[Price],
      IceCream.summon[Price]
    )
  )

@main def main(): Unit =
  var maybe: Option[Offering[Empty]] = None
  while (maybe.isEmpty) {
    val selection = StdIn.readLine()
    maybe = select(selection)
  }
  var offering: Offering[Option] = maybe.get.mapK(Empty.toOptionK)
  var customized: Offering[Id] = null
  while (customized == null) {
    val customization = StdIn.readLine()
    customize(offering)(customization) match
      case Left(c) => customized = c
      case Right(o) => offering = o
  }
  println(s"You selected: ${customized}")
  println(s"You owe: ${price(customized)}")
