package smusic

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiatonicScaleSpec extends AnyFlatSpec with TypeCheckedTripleEquals with Matchers {
  "Major.intervals" should "return 2212221" in {
    DiatonicScale.Major.intervals should === (Vector(2, 2, 1, 2, 2, 2 , 1))
  }

  "Dorian.intervals" should "return 2122212" in {
    DiatonicScale.Dorian.intervals should === (Vector(2, 1, 2, 2, 2 , 1, 2))
  }

  "Minor.intervals" should "return 2122122" in {
    DiatonicScale.Minor.intervals should === (Vector(2, 1, 2, 2, 1, 2, 2))
  }

  "Major.pitchClasses for tonic C" should "return CDEFGABC" in {
    DiatonicScale.Major.pitchClasses(tonic = PitchClass.PitchClass0).map(_.name).mkString("") should === (
      "CDEFGABC")
  }

  "Minor.pitchClasses for tonic A" should "return ABCDEFGA" in {
    DiatonicScale.Minor.pitchClasses(tonic = PitchClass.PitchClass9).map(_.name).mkString("") should === (
      "ABCDEFGA")
  }

  // TODO what's the deal with the circle of 5th and sharps? unit test this

}
