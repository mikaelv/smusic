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

  def perfectFifth: Option[Boolean] = {
    def _perfectFifth(i: IntMod12): Option[Boolean] = i.v match {
      case i if i == 7  => Some(true)
      case i if i == 6 => Some(false)
      case _ => None
    }
    // TODO algo could be used to find all components in one pass
    val (result, _) = intervals.is.foldLeft((Option.empty[Boolean], IntMod12(0))){ case ((fifth, accInterval), interval) =>
      if (fifth.isDefined)
        (fifth, accInterval + interval)
      else {
        accInterval match {
          case i if i.v < 6 => (None, accInterval + interval)
          case i if i.v > 7 => (None, accInterval + interval)
          case i => (_perfectFifth(i), accInterval + interval)
        }
      }
    }
    result
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

  override def toString: String = {
    val thirdSeventh = (thirdMajor, seventhMajor) match {
      case (Some(true), Some(true)) => "M7"
      case (Some(true), Some(false)) => "7"
      case (Some(true), None) => "M"
      case (Some(false), Some(false)) => "m7"
      case (Some(false), Some(true)) => ???
      case (Some(false), None) => "m"
      case (None, None) => ""
    }
    val fifth = perfectFifth match {
      case Some(true) => ""
      case Some(false) => "b5"
      case None => ???
    }
    thirdSeventh + fifth
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

