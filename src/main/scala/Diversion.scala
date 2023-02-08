package metalepsis

import cats.syntax.all.*

object Examples:
  val fibs = List(1, 2, 3, 5, 8, 13, 21)

  val fibsTransformed = fibs.map(x => (x * 10).toString)

  val fibsTransformed1_5: List[List[Int]] = fibs.map(x => 0.to(x).toList)

  val fibsTransformed1_75: List[Int] = fibs.map(x => 0.to(x).toList).flatten

  val fibsTransformed2: List[Int] = fibs.flatMap(x => 0.to(x).toList)

  val fibsTransformed3: List[Either[String, Int]] = fibs.map(x => (x % 2 == 0).guard[Option].as(x).toRight("Augh! Only give me evens!"))

  // List[Int]

  val fibsTransformed4: Either[String, List[Int]] = fibs.traverse(x => (x % 2 == 0).guard[Option].as(x).toRight("Augh! Only give me evens!"))

  val fibsTransformed5: Either[String, List[Int]] = fibsTransformed3.sequence

  val fibsTransformed6: Either[String, List[Int]] =
    fibs.map(x => (x % 2 == 0).guard[Option].as(x).toRight("Augh! Only give me evens!"))
      .foldLeft(Right(List.empty[Int])) { (s: Either[String, List[Int]], v: Either[String, Int]) =>
        (s, v).mapN((ss, vv) => vv :: ss) // TODO shouldn't reverse
      }

  val oneThing: Option[Int] = Some(21)

  val oneThing2: Either[String, Option[Int]] =
    oneThing.map(x => (x % 2 == 0).guard[Option].as(x).toRight("Augh! Only give me evens!"))
      .fold(Right(None))(_.map(Some(_)))

  // List[A]
  // A => IO[Unit]
  // IO[List[Unit]]
  // IO[Unit]
  // traverse_

  // Option[A]
