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
      KeySignature(IntMod8(12 - i.v), sharp = false)
    else
      KeySignature(IntMod8(i.v), sharp = true)

  }

}

case class KeySignature(count: IntMod8, sharp: Boolean) {
  override def toString: String = s"${count.v}${if (sharp) "#" else "b"}"
}
