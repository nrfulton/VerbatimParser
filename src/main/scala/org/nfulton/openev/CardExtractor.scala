package org.nfulton.openev

import spray.json._
import DefaultJsonProtocol._ // if you don't supply your own Protocol (see below)

object CardExtractor {
  type CardTrainingData = (String, String, String, String)

  /** Returns (original, byline, tagline, emphasized) */
  def apply(f: DebateFile): List[CardTrainingData] = f match {
    case TopLevelFile(name, cards) => cards.map(apply).flatten
    case Shell(name, cards) => cards.map(apply).flatten
    case RawCard(tag, byline, contents) => (originalOf(contents), byline, tag, cutOf(contents)) :: Nil
  }

  private def originalOf(s: String) = s.replaceAll("\u2029", "")

  private def cutOf(s: String) = {
    val sb = new StringBuilder()
    var include = !s.contains('\u2029') //if seps exist, don't start including. otherwise card isn't cut.
    s.toCharArray.foreach(c => {
      if(c == '\u2029') {
        include = !include
      }
      if(include) {
        sb.append(c)
      }
    })

    sb.toString()
  }

  def toJSON(d : CardTrainingData): String = d.toJson.prettyPrint
}
