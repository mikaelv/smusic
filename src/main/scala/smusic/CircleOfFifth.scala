package smusic

object CircleOfFifth {

  val head: PitchClass.PitchClass0.type = PitchClass.PitchClass0

  def apply(i: IntMod12): PitchClass = head + (i.v * 7)

  def indexOf(pc: PitchClass): IntMod12 = {
    @scala.annotation.tailrec
    def quotientModulo(dividend: Int, divisor: Int, modulo: Int): Int = {
      val remainder = dividend % divisor
      if (remainder == 0)
        (dividend / divisor) % modulo
      else
        quotientModulo(dividend + modulo, divisor, modulo)
    }
    IntMod12(quotientModulo(pc.i.v, 7, 12))
  }


  def keySignature(pc: PitchClass): KeySignature = {
    val i = CircleOfFifth.indexOf(pc)
    if (i.v > 6)
      KeySignature(IntMod7(12 - i.v), sharp = false)
    else
      KeySignature(IntMod7(i.v), sharp = true)

  }

  // TODO: circle of fourths explains how one tonal field (scale, key, chord) wants to move to become another tonal field
  // it wants to move that way because the scales are so close.
  // The 3rd od one scale, moves one semi tone to become the 7th of the next scale.
  // these 2 notes are important because they determine whetherthe scale is major, minor, dominant or dimished.



}

case class KeySignature(count: IntMod7, sharp: Boolean) {
  override def toString: String = s"${count.v}${if (sharp) "#" else "b"}"
}
