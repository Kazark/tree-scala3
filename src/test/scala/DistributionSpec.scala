package metalepsis

import cats.*
import cats.data.*
import cats.laws.discipline.*
import cats.syntax.all.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import munit.*

class DistributionSpec extends DisciplineSuite {
  import DistributionSpec.*
  checkAll(
    "Monad[Distribution]",
    MonadTests[Distribution].monad[String, Int, String],
  )
}

object DistributionSpec {
   def genDist[A](genA: Gen[A]): Gen[Distribution[A]] =
     Gen.listOf(genA).map(Distribution.uniform)

   implicit val arbDistInt: Arbitrary[Distribution[Int]] =
     Arbitrary(genDist(Gen.choose(0, 1000000000)))

   implicit val arbDistStr: Arbitrary[Distribution[String]] =
     Arbitrary(genDist(Gen.alphaNumStr))

   implicit val arbDistFSI: Arbitrary[Distribution[String => Int]] =
     Arbitrary(genDist(Arbitrary.arbitrary[String => Int]))

   implicit val arbDistFIS: Arbitrary[Distribution[Int => String]] =
     Arbitrary(genDist(Arbitrary.arbitrary[Int => String]))
}
