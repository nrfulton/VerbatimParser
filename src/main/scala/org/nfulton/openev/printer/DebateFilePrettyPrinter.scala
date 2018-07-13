package org.nfulton.openev.printer

import org.nfulton.openev.{DebateFile, RawCard, Shell, TopLevelFile}

object DebateFilePrettyPrinter {
  def asHTML(file: DebateFile):String = file match {
    case RawCard(title, byline, text) => s"<h3>${title}</h3><br/>${byline}<br/><span style='font-size: 3pt'>${text}</span>"
    case TopLevelFile(title, contents) => s"<title>${title}</title><h1>${title}</h1>" + contents.map(asHTML).reduce(_+_)
    case Shell(title, contents) => s"<h2>SHELL: ${title}</h2>" + contents.map(asHTML).reduce(_+_)
  }
}
