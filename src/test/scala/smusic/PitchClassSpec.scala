package smusic

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import smusic.PitchClass.{PitchClass0, PitchClassb}

class PitchClassSpec extends AnyFlatSpec with TypeCheckedTripleEquals with Matchers {
  "PitchClass.all" should "have pitch classes from 0 to 11" in {
    PitchClass.all.map(_.i.v) should contain theSameElementsInOrderAs (0 to 11)
  }

  "PitchClassb + 1" should "return PitchClass0" in {
    PitchClassb + 1 should === (PitchClass0)
  }

}
