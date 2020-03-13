package smusic

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