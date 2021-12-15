package metalepsis

import cats.*
import cats.data.*
import cats.syntax.all.*

type FromArgs[A] = PartialFunction[List[String], A]

trait Customize[F[_], A] extends Function[F[A], FromArgs[A]]:
  def apply(x: F[A]): FromArgs[A]

trait CustomizeEnum[A]:
  def customize: FromArgs[A]
  final def toField: CustomizeField[A] = _ => customize

object CustomizeEnum:
  def apply[A](using ev: CustomizeEnum[A]): CustomizeEnum[A] = ev

  given CustomizeEnum[String] with
    val customize: FromArgs[String] =
      case List(flavor) => flavor

trait CustomizeProduct[A] extends Customize[Id, A]

object CustomizeProduct:
  given [A]: Semigroup[CustomizeProduct[A]] with
    override def combine(
      l: CustomizeProduct[A],
      r: CustomizeProduct[A]
    ): CustomizeProduct[A] =
      (x: A) => l(x).orElse(r(x))

  def field[A, B: CustomizeField](
    tag: String,
    lens: FieldLens[FromArgs, A, Option[B]],
  ): CustomizeProduct[A] = (x: A) =>
    case arg :: args if arg === tag =>
      lens.update(
        x,
        CustomizeField[B](_).andThen(Some(_))
      )(args)

trait CustomizeField[A] extends Customize[Option, A]

object CustomizeField:
  def apply[A](using ev: CustomizeField[A]): CustomizeField[A] = ev

  given [A: CustomizeEnum]: CustomizeField[NonEmptyList[A]] with
    def apply(
      maybe: Option[NonEmptyList[A]]
    ): FromArgs[NonEmptyList[A]] =
      CustomizeEnum[A].customize.andThen { x =>
        maybe match
          case None => NonEmptyList.one(x)
          case Some(xs) => x :: xs
      }

  given [A: CustomizeEnum]: CustomizeField[List[A]] with
    def apply(maybe: Option[List[A]]): FromArgs[List[A]] =
      CustomizeEnum[A].customize.andThen { x =>
        maybe match
          case None => List(x)
          case Some(xs) => x :: xs
      }

  given [A: CustomizeEnum]: CustomizeField[Set[A]] with
    def apply(maybe: Option[Set[A]]): FromArgs[Set[A]] =
      CustomizeEnum[A].customize.andThen { x =>
        maybe match
          case None => Set(x)
          case Some(xs) => xs + x
      }

  given CustomizeField[String] with
    def apply(maybe: Option[String]): FromArgs[String] =
      case List(x) => x

final case class FieldLens[F[_], A, B](
  get: A => B,
  set: A => F[B] => F[A]
):
  def update(x: A, f: B => F[B]): F[A] =
    set(x)(f(get(x)))

object FieldLens:
  def fromArgs[A, B](
    get: A => B,
    set: B => A => A
  ): FieldLens[FromArgs, A, B] =
    FieldLens(get, x => _.andThen(set(_)(x)))
