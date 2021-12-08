package metalepsis

import cats.*
import cats.data.*

trait Offering[F[_]]:
  def mapK[G[_]](f: F ~> G): Offering[G]

  final def cancelWith[A](
    sandwich: Sandwich[[X] =>> F[X] => A],
    pizza: Pizza[[X] =>> F[X] => A],
    iceCream: IceCream[[X] =>> F[X] => A],
  ): List[A] =
    this match
      case o: Sandwich[F] => o.cancelK(sandwich)
      case o: Pizza[F] => o.cancelK(pizza)
      case o: IceCream[F] => o.cancelK(iceCream)

type Meat = String
type SandwichTopping = String
type Condiment = String

given shim: Price[String] = Price.included

final case class Sandwich[F[_]](
  bread: F[Bread],
  meat: F[Meat],
  toppings: F[List[SandwichTopping]],
  condiments: F[List[Condiment]],
) extends Offering[F]:
  override def mapK[G[_]](f: F ~> G): Offering[G] =
    Sandwich(
      f(bread),
      f(meat),
      f(toppings),
      f(condiments),
    )

given Price[List[SandwichTopping]] =
  toppings => {
    val extras = toppings.length - 2
    if extras > 0 then Money(extras * 0.15) else Monoid[Money].empty
  }

object Sandwich:
  given summon[F[_]](using
    bread: F[Bread],
    meat: F[Meat],
    toppings: F[List[SandwichTopping]],
    condiments: F[List[Condiment]],
  ): Sandwich[F] = Sandwich(bread, meat, toppings, condiments)

  given CancelK[Sandwich] with
    def cancelK[F[_], A](
      f: Sandwich[[X] =>> F[X] => A],
      x: Sandwich[F],
    ): List[A] =
      List(
        f.bread(x.bread),
        f.meat(x.meat),
        f.toppings(x.toppings),
        f.condiments(x.condiments),
      )

type Crust = String
type PizzaTopping = String

final case class Pizza[F[_]](
  crust: F[Crust],
  toppings: F[List[PizzaTopping]],
) extends Offering[F]:
  override def mapK[G[_]](f: F ~> G): Offering[G] =
    Pizza(f(crust), f(toppings))

object Pizza:
  given summon[F[_]](using
    crust: F[Crust],
    toppings: F[List[PizzaTopping]],
  ): Pizza[F] = Pizza(crust, toppings)

  given CancelK[Pizza] with
    def cancelK[F[_], A](
      f: Pizza[[X] =>> F[X] => A],
      x: Pizza[F],
    ): List[A] = ???

type Flavor = String

// enum Flavor:
//   case Vanilla
//   case Chocolate
//   case MintChocoChip
//   case ButterPecan
//   case CinnamonToastCrunch

type IceCreamTopping = String

// enum IceCreamTopping:
//   case Sprinkles
//   case Caramel
//   case HotFudge

// object IceCreamTopping:
//   given Price[IceCreamTopping] =
//     {
//       case IceCreamTopping.Sprinkles => Money(0.0)
//       case IceCreamTopping.Caramel => Money(0.25)
//       case IceCreamTopping.HotFudge => Money(0.25)
//     }

final case class IceCream[F[_]](
  scoops: F[NonEmptyList[Flavor]],
  toppings: F[Set[IceCreamTopping]],
) extends Offering[F]:
  override def mapK[G[_]](f: F ~> G): Offering[G] =
    IceCream(f(scoops), f(toppings))

given Price[NonEmptyList[Flavor]] =
  scoops => Money(scoops.length * 1.0)

given Price[Set[IceCreamTopping]] =
  toppings => Foldable[List].fold(toppings.toList.map(shim))

object IceCream:
  given summon[F[_]](using
    scoops: F[NonEmptyList[Flavor]],
    toppings: F[Set[IceCreamTopping]],
  ): IceCream[F] = IceCream(scoops, toppings)

  given CancelK[IceCream] with
    def cancelK[F[_], A](
      f: IceCream[[X] =>> F[X] => A],
      x: IceCream[F],
    ): List[A] =
      List(f.scoops(x.scoops), f.toppings(x.toppings))

  def customize(
    icecream: IceCream[Option]
  )(
    customization: String
  ): Either[IceCream[Id], IceCream[Option]] =
    customization.split(' ').toList match
      case List("scoop", flavor) =>
        val scoops =
           icecream.scoops match
             case None => NonEmptyList.one(flavor)
             case Some(flavors) => flavor :: flavors
        Right(icecream.copy(scoops = Some(scoops)))
      case List("topping", topping) =>
        val toppings =
           icecream.toppings match
             case None => Set(topping)
             case Some(ts) => ts + topping
        Right(icecream.copy(toppings = Some(toppings)))
      case List("done") =>
        icecream.scoops match
          case None => Right(icecream)
          case Some(flavors) =>
            Left(IceCream(flavors, icecream.toppings.getOrElse(Set.empty)))
      case _ => Right(icecream)
