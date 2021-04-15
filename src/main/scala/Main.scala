package metalepsis

import cats._
import cats.data._
import cats.syntax.all._

def safeDiv(numerator: Int, denominator: Int): Option[Int] =
  if denominator === 0
  then None
  else Some(numerator / denominator)

def pizzaParty(people: Int, pizzas: Int, slicePerPizza: Int): String =
  safeDiv(pizzas * slicePerPizza, people)
    .fold("No people, no party!")(s => s"$s slices per person")

def kSafeDiv[R](
  default: R
)(
  numerator: Int,
  denominator: Int
): Cont[R, Int] =
  if denominator === 0
  then Cont(_ => Eval(default))
  else Cont.pure(numerator / denominator)

def kPizzaParty(people: Int, pizzas: Int, slicePerPizza: Int): String =
  kSafeDiv("Infinite pizza available")(
    pizzas * slicePerPizza,
    people
  ).run(s => s"$s slices per person")

def range(min: Int, max: Int): List[Int] =
  min.to(max).toList

def permutations(max1: Int, max2: Int): List[(Int, Int)] =
  for {
    x <- range(0, max1)
    y <- range(0, max2)
  } yield (x, y)

def kRange[R](
  combine: (R, R) => R
)(
  min: Int,
  max: Int
): Cont[R, Int] =
  if min === max
  then Cont.pure(min)
  else Cont(k => combine(k(min), kRange(combine)(min+1, max)(k)))

def kPermutations[R](
  combine: (R, R) => R
)(
  max1: Int,
  max2: Int
): Cont[R, (Int, Int)] =
  for {
    x <- kRange(combine)(0, max1)
    y <- kRange(combine)(0, max2)
  } yield (x, y)

@main def main(): Unit =
  println(kPizzaParty(3, 5))
  println(permutations(0, 5).map(_.show).mkString(", "))
  println(kPermutations((x, y) => s"$x, $y")(0, 5).run(_.show))
