package org.nfulton.openev

import java.io.FileInputStream
import java.util.zip.ZipInputStream

object UnpackDocxFile {
  def apply(file: java.io.File): String = {
    val inputStream = new ZipInputStream(new FileInputStream(file))
    var next = inputStream.getNextEntry
    var docxml: String = null //yuck but intentionally staying consistent with next.
    while(next != null && docxml==null) {
      if(next.getName == "word/document.xml") {
        docxml = scala.io.Source.fromInputStream(inputStream).mkString
      }
      next = inputStream.getNextEntry
    }
    assert(docxml != null, s"did not find word/document.xml in ${file.getAbsoluteFile}")
    docxml
  }
}
