package smusic

case class IntMod12(v: Int) {
  require(v < 12 && v >= 0)

  def +(shift: Int): IntMod12 = IntMod12((v + shift) % 12)

  def name: Char =
    if (v < 10) ('0' + v).toChar
    else if (v == 10) 'a'
    else if (v == 11) 'b'
    else sys.error("invalid value: " + v)
}

// Rename / Alias to Note ? Maybe Note is too ambiguous: is it a pitch or a pitch class?
abstract class PitchClass(val i: IntMod12, val name: String, val altName: Option[String] = None) {
  def +(shift: Int): PitchClass = PitchClass.all((i + shift).v)

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

/* Heptatonic scale that includes 5 whole tones and 2 semitones */
case class DiatonicScale(degree: Degree) {
  def intervals: Vector[Int] = {
    val result = (DiatonicScale.MajorIntervals.drop(degree.v) ++ DiatonicScale.MajorIntervals.dropRight(7 - degree.v))
    assert(result.size == 7)
    result
  }

  def pitchClasses(tonic: PitchClass): Vector[PitchClass] =
    intervals.foldLeft(Vector(tonic)) { case (pcs, interval) =>
      pcs :+ (pcs.last + interval)
    }
}

object DiatonicScale {
  val MajorIntervals = Vector(2, 2, 1, 2, 2, 2, 1)

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
case class Degree(v: Int)

