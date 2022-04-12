package metalepsis

import cats.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*

enum CoinFlip:
  case Heads
  case Tails

object CoinFlip:
  def apply(heads: Boolean): CoinFlip =
    heads match
      case true => Heads
      case false => Tails

  def parse(raw: String): Option[CoinFlip] =
    raw match
      case "h" => Heads.some
      case "t" => Tails.some
      case _ => none

  given eq: Eq[CoinFlip] = Eq.by {
    case Heads => true
    case Tails => false
  }

final case class Round(guess: CoinFlip, actual: CoinFlip):
  import CoinFlip.*
  /** None = guessed correctly
    * Some(Heads) = you guessed Tails, but it was Heads
    * Some(Tails) = you guessed Heads, but it was Tails
    */
  def result: Result[Boolean] =
    Result(
      guessedHWasT = guess === Heads && actual === Tails,
      guessedTWasH = guess === Tails && actual === Heads,
      correctCount = guess === actual,
    )

final case class Result[A](
  guessedHWasT: A,
  guessedTWasH: A,
  correctCount: A,
)

object Result:
  private def totalWrong(result: Result[Int]): Int =
    result.guessedHWasT + result.guessedTWasH

  private def correctPercent(result: Result[Int]): Double =
    val total = result.correctCount + totalWrong(result)
    100.0 * result.correctCount / total

  def formatReport(result: Result[Int]): String =
    val line1 = s"You said heads, but it was tails, ${result.guessedHWasT} times"
    val line2 = s"You said tails, but it was heads, ${result.guessedTWasH} times"
    val line3 = f"Total percent correct: ${correctPercent(result)}%2.2f%%"
    s"$line1\n$line2\n$line3"

  given [A: Monoid]: Monoid[Result[A]] with
    override val empty: Result[A] =
      val empty = Monoid[A].empty
      Result(empty, empty, empty)
    override def combine(x: Result[A], y: Result[A]): Result[A] =
      Result(
        guessedHWasT = x.guessedHWasT |+| y.guessedHWasT,
        guessedTWasH = x.guessedTWasH |+| y.guessedTWasH,
        correctCount = x.correctCount |+| y.correctCount,
      )

  given Functor[Result] with
    def map[A, B](fa: Result[A])(f: A => B): Result[B] =
      Result(
        guessedHWasT = f(fa.guessedHWasT),
        guessedTWasH = f(fa.guessedTWasH),
        correctCount = f(fa.correctCount),
      )

object Game:
  def play[F[_]: Foldable](guesses: F[Round]): Result[Int] =
    guesses.foldMap(
      _.result.map {
        case true => 1
        case false => 0
      }
    )

  def randomFlip[F[_]: Random: Functor]: F[CoinFlip] =
    Random[F].nextBoolean.map(CoinFlip.apply)

  private def userGuesses_[F[_]: Console: Functor](
    acc: List[CoinFlip]
  ): F[Either[List[CoinFlip],List[CoinFlip]]] =
    Console[F].readLine.map {
      case ":q" => acc.asRight
      case input => (acc ++ CoinFlip.parse(input).toList).asLeft
    }

  def userGuesses[F[_]: Console: Monad]: F[List[CoinFlip]] =
    Monad[F].tailRecM(List.empty)(userGuesses_)

  def rounds[F[_]: Console: Random: Monad]: F[List[Round]] =
    userGuesses.flatMap(
      _.traverse { guess =>
        randomFlip.map { actual =>
          Round(guess = guess, actual = actual)
        }
      }
    )

  val playIO: IO[Unit] =
    for
      given Random[IO] <- Random.scalaUtilRandom[IO]
      rounds_ <- rounds[IO]
      report = Result.formatReport(play(rounds_))
      _ <- Console[IO].println(report)
    yield ()

@main def main(): Unit =
  import cats.effect.unsafe.implicits.global
  Game.playIO.unsafeRunSync()
