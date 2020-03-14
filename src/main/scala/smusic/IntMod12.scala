package smusic

import scala.language.implicitConversions

case class IntMod12(v: Int) {
  require(v < 12 && v >= 0)

  def +(shift: IntMod12): IntMod12 = IntMod12((v + shift.v) % 12)
  def -(shift: IntMod12): IntMod12 = IntMod12((v - shift.v + 12) % 12)

  def name: Char =
    if (v < 10) ('0' + v).toChar
    else if (v == 10) 'a'
    else if (v == 11) 'b'
    else sys.error("invalid value: " + v)
}

object IntMod12 {
  implicit def intToIntMod12(i: Int): IntMod12 = IntMod12(i % 12)
}


case class IntMod7(v: Int) {
  require(v < 8 && v >= 0)

  def +(shift: IntMod7): IntMod7 = IntMod7((v + shift.v) % 7)
}

object IntMod7 {
  implicit def intToIntMod7(i: Int): IntMod7 = IntMod7(i % 7)

}