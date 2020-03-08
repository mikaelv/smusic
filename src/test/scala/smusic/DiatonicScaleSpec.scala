package smusic

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiatonicScaleSpec extends AnyFlatSpec with TypeCheckedTripleEquals with Matchers {
  "Major.intervals" should "return 2212221" in {
    DiatonicScale.Major.intervals.is should ===(Vector[IntMod12](2, 2, 1, 2, 2, 2, 1))
  }

  "Dorian.intervals" should "return 2122212" in {
    DiatonicScale.Dorian.intervals.is should ===(Vector[IntMod12](2, 1, 2, 2, 2, 1, 2))
  }

  "Minor.intervals" should "return 2122122" in {
    DiatonicScale.Minor.intervals.is should ===(Vector[IntMod12](2, 1, 2, 2, 1, 2, 2))
  }

  "Major.pitchClasses for tonic C" should "return CDEFGABC" in {
    DiatonicScale.Major.intervals.pitchClasses(tonic = PitchClass.PitchClass0).map(_.name).mkString("") should ===(
      "CDEFGABC")
  }

  "Minor.pitchClasses for tonic A" should "return ABCDEFGA" in {
    DiatonicScale.Minor.intervals.pitchClasses(tonic = PitchClass.PitchClass9).map(_.name).mkString("") should ===(
      "ABCDEFGA")
  }

  "Major.triad" should "be Major" in {
    DiatonicScale.Major.triad should === (ChordClass.TriadMajor)
  }

  "Major.tetrad" should "be Major7" in {
    DiatonicScale.Major.tetrad should === (ChordClass.TetradMajor7)
  }

  "Lydian.tetrad" should "be Major7" in {
    DiatonicScale.Lydian.tetrad should === (ChordClass.TetradMajor7)
  }

  "Mixolydian.tetrad" should "be Major7" in {
    DiatonicScale.Mixolydian.tetrad should === (ChordClass.Tetrad7)
  }


  // TODO what's the deal with the circle of 5th and sharps? unit test this

}
