package smusic

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers

class CircleOfFifthSpec extends org.scalatest.FlatSpec with TypeCheckedTripleEquals with Matchers {
  "CircleOfFifth(0) to CircleOfFifth(11)" should "be C G D A E B F# C# G# D# A# F" in {
    Seq.tabulate(12)(i => CircleOfFifth(i).name) should === ("C G D A E B F# C# G# D# A# F".split(" ").toSeq)
  }

  "CircleOfFifth(0) to CircleOfFifth(11) alt names" should "be C G D A E B Gb Db Ab Eb Bb F" in {
    Seq.tabulate(12)(i => CircleOfFifth(i).altName) should === ("C G D A E B Gb Db Ab Eb Bb F".split(" ").toSeq)
  }

  "CircleOfFifth.indexOf" should "be the reciprocal function of apply" in {
    val actual = Seq.tabulate(12)(i => CircleOfFifth.indexOf(PitchClass(i)))
    val expect = Seq.tabulate(12)(i => CircleOfFifth.apply(i).i)
    actual should === (expect)
  }

  "CircleOfFifth.keySignature" should "be in number of sharps until position 6, and in number of flats after" in {
    CircleOfFifth.keySignature(PitchClass.PitchClassB).toString should === ("5#")
    CircleOfFifth.keySignature(PitchClass.PitchClassFs).toString should === ("6#")
    CircleOfFifth.keySignature(PitchClass.PitchClassCs).toString should === ("5b")

    Seq.tabulate(12)(i => CircleOfFifth.keySignature(CircleOfFifth(i))) should === (
      Seq.tabulate(7)(i => KeySignature(IntMod7(i), sharp = true)) ++
        (5 to 1 by -1).map(i => KeySignature(IntMod7(i), sharp = false))
    )
  }
}
