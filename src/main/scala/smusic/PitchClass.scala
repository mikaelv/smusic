package smusic

import scala.language.implicitConversions




// Rename / Alias to Note ? Maybe Note is too ambiguous: is it a pitch or a pitch class?
abstract class PitchClass(val i: IntMod12, val name: String, val altName: String) {
  def +(shift: IntMod12): PitchClass = PitchClass.all((i + shift).v)

  override def toString: String = "pc" + i.name.toString


}

object PitchClass {

  case object PitchClass0 extends PitchClass(IntMod12(0), "C", "C")

  case object PitchClass1 extends PitchClass(IntMod12(1), "C#", "Db")

  case object PitchClass2 extends PitchClass(IntMod12(2), "D", "D")

  case object PitchClass3 extends PitchClass(IntMod12(3), "D#", "Eb")

  case object PitchClass4 extends PitchClass(IntMod12(4), "E", "E")

  case object PitchClass5 extends PitchClass(IntMod12(5), "F", "F")

  case object PitchClass6 extends PitchClass(IntMod12(6), "F#", "Gb")

  case object PitchClass7 extends PitchClass(IntMod12(7), "G", "G")

  case object PitchClass8 extends PitchClass(IntMod12(8), "G#", "Ab")

  case object PitchClass9 extends PitchClass(IntMod12(9), "A", "A")

  case object PitchClass10 extends PitchClass(IntMod12(10), "A#", "Bb")

  case object PitchClass11 extends PitchClass(IntMod12(11), "B", "B")

  // aliases
  val PitchClassC = PitchClass0
  val PitchClassCs = PitchClass1
  val PitchClassDb = PitchClass1
  val PitchClassD = PitchClass2
  val PitchClassDs = PitchClass3
  val PitchClassEb = PitchClass3
  val PitchClassE = PitchClass4
  val PitchClassF = PitchClass5
  val PitchClassFs = PitchClass6
  val PitchClassGb = PitchClass6
  val PitchClassG = PitchClass7
  val PitchClassGs = PitchClass8
  val PitchClassAb = PitchClass8
  val PitchClassA = PitchClass9
  val PitchClassAs = PitchClass10
  val PitchClassBb = PitchClass10
  val PitchClassB = PitchClass11

  lazy val all: Vector[PitchClass] = Vector(
    PitchClass0, PitchClass1, PitchClass2, PitchClass3, PitchClass4, PitchClass5, PitchClass6,
    PitchClass7, PitchClass8, PitchClass9, PitchClass10, PitchClass11
  )

  def apply(i: IntMod12): PitchClass = all(i.v)
}








// TODO express in roman numerals
case class Degree(v: IntMod8)

