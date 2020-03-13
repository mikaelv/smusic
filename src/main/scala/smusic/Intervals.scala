package smusic

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