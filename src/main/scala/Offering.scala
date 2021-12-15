package metalepsis

import cats.*
import cats.data.*
import cats.syntax.all.*

sealed trait Offering[F[_]]:
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

  final def fold[A](
    sandwich: Sandwich[F] => A,
    pizza: Pizza[F] => A,
    iceCream: IceCream[F] => A,
  ): A =
    this match
      case o: Sandwich[F] => sandwich(o)
      case o: Pizza[F] => pizza(o)
      case o: IceCream[F] => iceCream(o)

object Offering:
  val prompt = List(Sandwich.Tag, Pizza.Tag, IceCream.Tag)

  def done(offering: Offering[Option]): Option[Offering[Id]] =
    offering.fold(
      Sandwich.done,
      Pizza.done,
      IceCream.done,
    )

  val customize: CustomizeProduct[Offering[Option]] =
    _.fold(
      Sandwich.customize,
      Pizza.customize,
      IceCream.customize,
    )

trait OfferingModule[O[_[_]]]:
  val Tag: String
  val cancelK: CancelK[O]
  def done(o: O[Option]): Option[O[Id]]
  type L[F[_], A] = FieldLens[FromArgs, O[F], F[A]]
  def lenses[F[_]]: O[[X] =>> L[F, X]]
  val customize: CustomizeProduct[O[Option]]

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

object Sandwich extends OfferingModule[Sandwich]:
  val Tag: String = "sandwich"

  given summon[F[_]](using
    bread: F[Bread],
    meat: F[Meat],
    toppings: F[List[SandwichTopping]],
    condiments: F[List[Condiment]],
  ): Sandwich[F] = Sandwich(bread, meat, toppings, condiments)

  given cancelK: CancelK[Sandwich] with
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

  def done(sandwich: Sandwich[Option]): Option[Sandwich[Id]] =
    (sandwich.bread, sandwich.meat).mapN {
      Sandwich(
        _,
        _,
        sandwich.toppings.getOrElse(List.empty),
        sandwich.condiments.getOrElse(List.empty),
      )
    }

  type L[F[_], A] = FieldLens[FromArgs, Sandwich[F], F[A]]

  def lenses[F[_]]: Sandwich[[X] =>> L[F, X]] =
    apply(
      FieldLens.fromArgs(_.bread, y => _.copy(bread = y)),
      FieldLens.fromArgs(_.meat, y => _.copy(meat = y)),
      FieldLens.fromArgs(_.toppings, y => _.copy(toppings = y)),
      FieldLens.fromArgs(_.condiments, y => _.copy(condiments = y)),
    )

  val customize: CustomizeProduct[Sandwich[Option]] =
    CustomizeProduct.field("bread", lenses.bread) |+|
    CustomizeProduct.field("meat", lenses.meat) |+|
    CustomizeProduct.field("toppings", lenses.toppings) |+|
    CustomizeProduct.field("condiments", lenses.condiments)

type Crust = String
type PizzaTopping = String

final case class Pizza[F[_]](
  crust: F[Crust],
  toppings: F[List[PizzaTopping]],
) extends Offering[F]:
  override def mapK[G[_]](f: F ~> G): Offering[G] =
    Pizza(f(crust), f(toppings))

object Pizza extends OfferingModule[Pizza]:
  val Tag = "pizza"

  given summon[F[_]](using
    crust: F[Crust],
    toppings: F[List[PizzaTopping]],
  ): Pizza[F] = Pizza(crust, toppings)

  given cancelK: CancelK[Pizza] with
    def cancelK[F[_], A](
      f: Pizza[[X] =>> F[X] => A],
      x: Pizza[F],
    ): List[A] =
      List(f.crust(x.crust), f.toppings(x.toppings))

  def done(pizza: Pizza[Option]): Option[Pizza[Id]] =
    pizza.crust.map {
      Pizza(_, pizza.toppings.getOrElse(List.empty))
    }

  type L[F[_], A] = FieldLens[FromArgs, Pizza[F], F[A]]

  def lenses[F[_]]: Pizza[[X] =>> L[F, X]] =
    apply(
      FieldLens.fromArgs(_.crust, y => _.copy(crust = y)),
      FieldLens.fromArgs(_.toppings, y => _.copy(toppings = y)),
    )

  val customize: CustomizeProduct[Pizza[Option]] =
    CustomizeProduct.field("crust", lenses.crust) |+|
      CustomizeProduct.field("topping", lenses.toppings)

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

object IceCream extends OfferingModule[IceCream]:
  val Tag = "ice-cream"

  given summon[F[_]](using
    scoops: F[NonEmptyList[Flavor]],
    toppings: F[Set[IceCreamTopping]],
  ): IceCream[F] = IceCream(scoops, toppings)

  given cancelK: CancelK[IceCream] with
    def cancelK[F[_], A](
      f: IceCream[[X] =>> F[X] => A],
      x: IceCream[F],
    ): List[A] =
      List(f.scoops(x.scoops), f.toppings(x.toppings))

  def done(icecream: IceCream[Option]): Option[IceCream[Id]] =
    icecream.scoops.map(
      IceCream(_, icecream.toppings.getOrElse(Set.empty))
    )

  type L[F[_], A] = FieldLens[FromArgs, IceCream[F], F[A]]

  def lenses[F[_]]: IceCream[[X] =>> L[F, X]] =
    apply(
      FieldLens.fromArgs(_.scoops, y => _.copy(scoops = y)),
      FieldLens.fromArgs(_.toppings, y => _.copy(toppings = y)),
    )

  val customize: CustomizeProduct[IceCream[Option]] =
    CustomizeProduct.field("scoop", lenses.scoops) |+|
      CustomizeProduct.field("topping", lenses.toppings)
