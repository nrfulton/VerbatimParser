package org.nfulton.openev

import java.io.{FileWriter, PrintWriter}

import org.nfulton.openev.printer.DebateFilePrettyPrinter

object BatchProcessor {
  /** If none, output each file separately. */
  val outputFile : Option[String] = None

  def main(argv: Array[String]):Unit = {
    val inputDirectory = "/home/nfulton/db8/"
    val d = new java.io.File(inputDirectory)

    assert(d.exists())
    assert(d.isDirectory)

    var totalCount = 0
    var nonDocXcount = 0
    var cantParseCount = 0

    d.listFiles.filter(!_.getName.contains("lock")).filter(!_.getName.endsWith("html")).foreach(f => {
      totalCount += 1
      if(!f.getName.toLowerCase().endsWith("docx")) {
        nonDocXcount += 1
        println(s"Cannot process non-docx file named: ${f.getAbsoluteFile}")
      }
      else {
        try {
          val xmlContents = UnpackDocxFile(f)
          val file = EvidenceParser(xmlContents)
//          dumpCards(file)
          outputFile match {
            case None =>
              dump(s"${f.getAbsoluteFile}.html", DebateFilePrettyPrinter.asHTML(file))
            case Some(fullFile) =>
              dump(fullFile, DebateFilePrettyPrinter.asHTML(file))
          }
        }
        catch {
          case e: Throwable => {
            cantParseCount += 1
            println(s"Failed to parse docx file ${f.getAbsoluteFile} for reason ${e.getMessage}")
          }
        }
      }
    })

    println(s"summary: ${totalCount} files with ${nonDocXcount} non-docx files and ${cantParseCount} docx files I couldn't parse as evidence. Total amount of evidence is ${cardCount}")
  }

  private def dump(f: String, contents:String) =
    Some(new PrintWriter(f)).foreach{p => p.write(contents); p.close}


  var cardCount = 0
  /** Extracts cards in format specified in [[CardExtractor]] for the purpose of training auto-cutters. */
  private def dumpCards(file: DebateFile): Unit = {
    val fw = new FileWriter("/home/nfulton/cards.json", true)
    dumpCards(fw, file)
    fw.close()
  }
  private def dumpCards(fw: FileWriter, file: DebateFile):Unit = {
    file match {
      case TopLevelFile(_, contents) => contents.map(dumpCards(fw,_))
      case Shell(_, contents) => contents.map(dumpCards(fw,_))
      case RawCard(tag,byline,contents) => if(contents != "") {
        val trainingData = CardExtractor(file)
        trainingData
          .filter(_._1 != "")
          .map(CardExtractor.toJSON)
          .foreach(s => {
            fw.write(s + ",\n")
          })
        println(CardExtractor(file))
        cardCount += 1
      }
    }
  }
}
