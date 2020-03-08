package smusic

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import smusic.PitchClass.{PitchClass0, PitchClassb}

class ChordClassSpec extends AnyFlatSpec with TypeCheckedTripleEquals with Matchers {
  "ChordClass.TetradMajor7" should "have a major 3rd and a major 7th" in {
    ChordClass.TetradMajor7.thirdMajor should === (Some(true))
    ChordClass.TetradMajor7.seventhMajor should === (Some(true))
  }

  "ChordClass.TetradMinor7" should "have a minor 3rd and a minor 7th" in {
    ChordClass.TetradMinor7.thirdMajor should === (Some(false))
    ChordClass.TetradMinor7.seventhMajor should === (Some(false))
  }

  "ChordClass.Tetrad7" should "have a major 3rd and a minor 7th" in {
    ChordClass.Tetrad7.thirdMajor should === (Some(true))
    ChordClass.Tetrad7.seventhMajor should === (Some(false))
  }

  "ChordClass.TriadMajor" should "have a major 3rd and no 7th" in {
    ChordClass.TriadMajor.thirdMajor should === (Some(true))
    ChordClass.TriadMajor.seventhMajor should === (None)
  }

  "ChordClass.TriadMinor" should "have a minor 3rd and no 7th" in {
    ChordClass.TriadMinor.thirdMajor should === (Some(false))
    ChordClass.TriadMinor.seventhMajor should === (None)
  }

}
