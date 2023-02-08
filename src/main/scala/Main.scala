package metalepsis

import cats.*
import cats.syntax.all.*

final case class Record[K, V](
  key: K,
  value: V,
  metadata: String, // punt
)

object Record:
  def persist[F[_]](record: Record[String, String]): F[Unit] = ??? // saves to datastore

  def retrieve[F[_]](key: String): F[Record[String, String]] = ??? // saves to datastore

  def parseKey[F[_]: ApplicativeThrow, K](key: String): F[K] = ???
  def parseValue[F[_]: ApplicativeThrow, V](value: String): F[V] = ???

  implicit val bitraverse: Bitraverse[Record] =
    new Bitraverse[Record] {
      override def bifoldLeft[A, B, C](
        fab: Record[A, B],
        c: C,
      )(
        f: (C, A) => C,
        g: (C, B) => C,
      ): C = {
        val a = fab.key
        val b = fab.value
        g(f(c, a), b)
      }

      override def bifoldRight[A, B, C](
        fab: Record[A, B],
        c: Eval[C],
      )(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C],
      ): Eval[C] = {
        val a = fab.key
        val b = fab.value
        f(a, g(b, c))
      }

      override def bitraverse[G[_]: Applicative, A, B, C, D](
        fab: Record[A, B]
      )(
        f: A => G[C],
        g: B => G[D],
      ): G[Record[C, D]] =
        ( f(fab.key)
        , g(fab.value)
        ).mapN((c, d) => fab.copy(key = c, value = d))
    }

  // IO[List[A]]
  // IO: Applicative, not Traverse
  // List has both
  // List[IO[A]] => IO[List[A]]

  def retrieveParsed[F[_]: MonadThrow, K, V](key: String): F[Record[K, V]] =
    retrieve[F](key).flatMap(_.bitraverse(parseKey[F, K](_), parseValue[F, V](_)))

@main def main(): Unit =
  println("Hello, world!")
