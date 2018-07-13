package org.nfulton.openev

trait DebateFile {
  def addCard(c: RawCard) = this match {
    case TopLevelFile(name, contents) => TopLevelFile(name, contents :+ c)
    case Shell(name, contents) => Shell(name, contents :+ c)
    case Frontline(name, contents) => Frontline(name, contents :+ c)
    case rc: RawCard => ???
  }
}

case class TopLevelFile(val name: String, val contents: List[DebateFile]) extends DebateFile

case class Shell(name: String, contents: List[RawCard]) extends DebateFile

case class Frontline(name: String, contents: List[RawCard]) extends DebateFile

/**
  *
  * @param tag @todo give structure (uniqueness, link, impact)
  * @param byLine @todo extract date, author, source
  * @param text @todo allow text to be formated.
  */
case class RawCard(tag: String, byLine: String, text: String) extends DebateFile