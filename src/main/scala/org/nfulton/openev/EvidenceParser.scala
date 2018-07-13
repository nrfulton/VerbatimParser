package org.nfulton.openev

import java.io.{File, PrintWriter}

import org.nfulton.openev.printer.DebateFilePrettyPrinter

import scala.xml._


object EvidenceParser {
  //region Type definitions
  private object PARSER_STATE_ENUM extends Enumeration {
    type ParserState = Value
    val seeking, tagFound, bylineFound = Value
  }
  import PARSER_STATE_ENUM._

  private type ParsedCard = (Option[String], Option[String], Option[String])
  //endregion


  def apply(file:File): DebateFile = {
    apply(scala.io.Source.fromFile(file).mkString)
  }

  def apply(s:String) : DebateFile = {
    val document = XML.loadString(s)

    var debateFile : DebateFile = TopLevelFile("Name Unknown", Nil)
    var currentState : ParserState = seeking
    var currentCard : ParsedCard = (None,None,None)

    (document \\ "p").map(node => {
      //Detecting a new heading automatically resets the parser state appropriately.
      if(isTagline(node)) {
        if(currentState == seeking) {
          if(currentCard._1 isDefined)
            debateFile = addCard(currentCard, debateFile)
          currentState = tagFound
          log(s"Tagline: ${node.text}")
          currentCard = (Some(node.text), None, None)
        }
        else if(currentState == tagFound) {
          log(s"Tagline: ${node.text}")
          assert(currentCard._1 isDefined, "expected for the current tag to be a continuation of the prior")
          currentCard = (Some(currentCard._1.get + node.text), None, None)
        }
        else if(currentState == bylineFound) {
          //@todo found a byline but not a card. I guess that means this must be a plantext, analysis, or something along those lines.
          //@todo come up with a way of marking portions of a file for cleanup.
          debateFile = addCard((currentCard._1, Some(""), currentCard._2), debateFile)
          log("[TODO] handle the todos around line 50 of the parser")
          currentState = seeking
        }
      }
      //If we've already started reading from a tagline, then it's time to start searching for the byline and contents line.
      else if(currentState == tagFound) {
        if (isNonEmptyString(node.text)) {
          val byline = formattedString(node)
          log(s"byline: ${byline}")
          currentCard = (currentCard._1, Some(byline), None)
          currentState = bylineFound
        }
        else {
          //found only whitespace, continue...
          currentState = currentState
        }
      }
      else if(currentState == bylineFound) {
        //@note currentCard._3 might not be None.
        val cardContents = formattedString(node)
        currentCard._3 match {
          case None => currentCard = (currentCard._1, currentCard._2, Some(cardContents))
          case Some(prev) => currentCard = (currentCard._1, currentCard._2, Some(prev+cardContents))
        }

      }
    })
    debateFile
  }

  private val DEBUG = false
  private def log(s:String) = {
    val msg = s"[ParserLog] ${s}"
    if(DEBUG) println(msg)
  }

  private def addCard(c : ParsedCard, section: DebateFile) = {
    //@todo for higher-quality dataset we need these assertions. for now just harvest all the data we can.
//    assert(c._1.isDefined, "title should be defined")
//    assert(c._2.isDefined, s"byline should be defined for ${c._1}")
//    assert(c._3.isDefined, s"contents should be defined for ${c._1} ${c._2}")
    if(c._1.isDefined && c._2.isDefined && c._3.isDefined) {
      val cardToAdd = RawCard(c._1.get, c._2.get, c._3.get)
      section.addCard(cardToAdd)
    }
    else {
      section //@todo for higher-quality dataset we should never get here.
    }
  }

  private def formattedString(node: Node):String = {
    val regions = (node \\ "r")
    if(regions nonEmpty)
      regions.map(n => formattedLeafNode(n)).reduceRight((s, b) => s + b)
    else formattedLeafNode(node)
  }

  /**
    * @note this is where the definition of various styles is given...
    */
  private def formattedLeafNode(node: Node) = {
    val formattingFunctions =
      ifWrap(isUnderline(node) || isBold(node) || isEmph(node), "\u2029", "\u2029") ::
//      ifWrap(isUnderline(node), "<span style='text-decoration: underline; font-size: 12pt'>", "</span>") ::
//      ifWrap(isBold(node), "<span style='font-weight: bold'>", "</span>") ::
//      ifWrap(isEmph(node), "<span style='text-decoration: underline; font-size: 12pt'>", "</span>") :: //@todo
      Nil

    formattingFunctions.foldLeft(node.text)((str, nxt) => nxt(str))
  }

  private def ifWrap(condition: Boolean, start: String, end:String) =
    if(condition) ((s:String) => start+s+end)
    else ((s:String) => s)

  private def isBold(n:Node): Boolean =
    (n \\ "rPr" \\ "rStyle").headOption match {
      case Some(value) => value.attributes.asAttrMap.get("w:val") match {
        case Some(styleName) => styleName == "Style13ptBold"
        case None => false
      }
      case None => false
    }

  private def isEmph(n:Node): Boolean =
    (n \\ "rPr" \\ "rStyle").headOption match {
      case Some(value) => value.attributes.asAttrMap.get("w:val") match {
        case Some(styleName) => styleName == "Emphasis"
        case None => false
      }
      case None => false
    }

  private def isUnderline(n:Node): Boolean =
    (n \\ "rPr" \\ "rStyle").headOption match {
      case Some(value) => value.attributes.asAttrMap.get("w:val") match {
        case Some(styleName) => styleName == "StyleUnderline"
        case None => false
      }
      case None => false
    }



  /** returns true iff string contains non-whitespace characters. */
  private def isNonEmptyString(s:String): Boolean = {
    s.replaceAll("\\s", "").nonEmpty
  }

  /** Gets the paragraph style? */
  private def getParagraphStyle(node: Node) = {
    (node \\ "pPr" \\ "pStyle").headOption match {
      case Some(value) => value.attributes.asAttrMap.get("w:val")
      case None => None
    }
  }

  /** Returns true if this is a heading. */
  private def isTagline(node: Node) = getParagraphStyle(node) match {
    case Some(value) if(value == "Heading4") => true
    case _ => false
  }

  private def getCardContents(node: Node): String = {
    (node \\ "t").map(child => child.text).reduce(_+_)
  }

  def main(argv: Array[String]): Unit =
    this.apply(new java.io.File("/home/nfulton/dev/rand/xmldb8/src/main/resources/document.xml"))
}
