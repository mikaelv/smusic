package smusic

import scala.collection.SortedSet

case class Chord(pc: PitchClass, cc: ChordClass)
case class ChordClass(intervals: Intervals) {
  /* @return true if the third is a major third, false if it is a minor third, None if there is no third */
  def thirdMajor: Option[Boolean] = {
    require(intervals.is.length >= 2)
    def _thirdMajor(i: IntMod12): Option[Boolean] = i.v match {
      case i if i == 4  => Some(true)
      case i if i == 3 => Some(false)
      case _ => None
    }

    intervals.is.head.v match {
      case i if i < 3 => _thirdMajor(i + intervals.is(1))
      case i if i > 4 => None
      case i => _thirdMajor(i)
    }
  }

  /* @return true if the third is a major 7th, false if it is a minor 7th, None if there is no 7th */
  def seventhMajor: Option[Boolean] = {
    require(intervals.is.nonEmpty)
    def _seventhMajor(i: IntMod12): Option[Boolean] = i.v match {
      case i if i == 11  => Some(true)
      case i if i == 10 => Some(false)
      case _ => None
    }

    _seventhMajor(IntMod12(0) - intervals.is.last.v)
  }
}

object ChordClass {
  val TriadMajor: ChordClass = ChordClass(Intervals(4, 3, 5))
  val TriadMinor: ChordClass = ChordClass(Intervals(3, 4, 5))
  val TetradMajor7: ChordClass = ChordClass(Intervals(4, 3, 4, 1))
  val TetradMinor7: ChordClass = ChordClass(Intervals(3, 4, 3, 2))
  val Tetrad7: ChordClass = ChordClass(Intervals(4, 3, 3, 2))
  val TetradMin7b5: ChordClass = ChordClass(Intervals(3, 3, 4, 2))

}

