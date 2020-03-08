package smusic

import scala.language.implicitConversions




// Rename / Alias to Note ? Maybe Note is too ambiguous: is it a pitch or a pitch class?
abstract class PitchClass(val i: IntMod12, val name: String, val altName: Option[String] = None) {
  def +(shift: IntMod12): PitchClass = PitchClass.all((i + shift).v)

  override def toString: String = "pc" + i.name.toString


}

object PitchClass {

  case object PitchClass0 extends PitchClass(IntMod12(0), "C")

  case object PitchClass1 extends PitchClass(IntMod12(1), "C#", Some("Db"))

  case object PitchClass2 extends PitchClass(IntMod12(2), "D")

  case object PitchClass3 extends PitchClass(IntMod12(3), "D#", Some("Eb"))

  case object PitchClass4 extends PitchClass(IntMod12(4), "E")

  case object PitchClass5 extends PitchClass(IntMod12(5), "F")

  case object PitchClass6 extends PitchClass(IntMod12(6), "F#", Some("Gb"))

  case object PitchClass7 extends PitchClass(IntMod12(7), "G")

  case object PitchClass8 extends PitchClass(IntMod12(8), "G#", Some("Ab"))

  case object PitchClass9 extends PitchClass(IntMod12(9), "A")

  case object PitchClassa extends PitchClass(IntMod12(10), "A#", Some("Bb"))

  case object PitchClassb extends PitchClass(IntMod12(11), "B")

  lazy val all: Vector[PitchClass] = Vector(
    PitchClass0, PitchClass1, PitchClass2, PitchClass3, PitchClass4, PitchClass5, PitchClass6,
    PitchClass7, PitchClass8, PitchClass9, PitchClassa, PitchClassb
  )
}

case class Intervals(is: Vector[IntMod12]) {
  require(is.map(_.v).sum == 12, s"intervals sum: ${is.map(_.v).sum}, expected: 12")

  def apply(index: Int): IntMod12 = is.apply(index)

  def rotate(start: Int): Intervals =
    Intervals(is.drop(start) ++ is.dropRight(is.size - start))


  def pitchClasses(tonic: PitchClass): Vector[PitchClass] =
    is.foldLeft(Vector(tonic)) { case (pcs, interval) =>
      pcs :+ (pcs.last + interval)
    }

  // TODO name for this concept? looks similar to pitchClass without reference to a pitch
  lazy val shifts: Vector[IntMod12] = is.foldLeft(Vector(is.head)){ case (acc, interval) =>
    acc :+ (is.last + interval)
  }

  def heptatonic: Boolean = is.size == 7
}
object Intervals {
  def apply(is: IntMod12*): Intervals = Intervals(is.toVector)
}

// TODO model a Scale as case class Scale(Degree, Intervals) ?
/* Heptatonic scale that includes 5 whole tones and 2 semitones */
case class DiatonicScale(degree: Degree) {
  lazy val intervals: Intervals = {
    val result = DiatonicScale.MajorIntervals.rotate(degree.v.v)
    assert(result.heptatonic)
    result
  }

  def trichord: ChordClass = triad
  lazy val triad: ChordClass = {
    ChordClass(Intervals(
      intervals(0) + intervals(1),
      intervals(2) + intervals(3),
      intervals(4) + intervals(5) + intervals(6)
    ))
  }

  def tetrachord: ChordClass = tetrad
  lazy val tetrad: ChordClass = {
    ChordClass(Intervals(
      intervals(0) + intervals(1),
      intervals(2) + intervals(3),
      intervals(4) + intervals(5),
      intervals(6)
    ))
  }
}

object DiatonicScale {
  val MajorIntervals: Intervals = Intervals(2, 2, 1, 2, 2, 2, 1)

  val Ionian = DiatonicScale(Degree(0))
  val Dorian = DiatonicScale(Degree(1))
  val Phrygian = DiatonicScale(Degree(2))
  val Lydian = DiatonicScale(Degree(3))
  val Mixolydian = DiatonicScale(Degree(4))
  val Aeolian = DiatonicScale(Degree(5))
  val Locrian = DiatonicScale(Degree(6))


  val Major = Ionian
  val Minor = Aeolian
}

// TODO express in roman numerals
case class Degree(v: IntMod8)

