package com.gu.contentapi.difftool

import io.Source

import dispatch._
import OwenDiff.{Diff => OwensDiff}
import net.liftweb.json._
import java.io.{FileWriter, File, StringWriter}
import net.liftweb.json.JsonAST.JValue
import xml.{Elem, Node, XML}
import xml.transform.{RuleTransformer, RewriteRule}



object Main extends App with FileHelpers {
  val masterContentApiHost = url("http://localhost:8080/api")
  val liftRestContentApiHost = url("http://localhost:8700/content-api/api")

  val h = new Http with thread.Safety with NoLogging


  def getResponse(req: Request) = {
    h((req <:< Map("Accept" -> "application/json")) as_str)
  }

  def diffResult(master: String, liftRest: String) = {
    master.charAt(0) match {
      case '<' => XmlDiff.diff(master, liftRest)
      case _ => JsonDiff.diff(master, liftRest)
    }
  }

  def processline(idxAndLine: (String, Int)) {
    val (line, idx) = idxAndLine

    val pathAndParams = line.replaceAllLiterally("/content-api/api/", "").split("/").filterNot( _ == "").mkString("/")

    try {
      // Translate the URLs into calls to make to the:
      // 1. Master Content Api
      val masterResponse = getResponse(masterContentApiHost / pathAndParams)

      // 2. Lift-rest Content Api
      val liftRestResponse = getResponse(liftRestContentApiHost / pathAndParams)

      diffResult(masterResponse, liftRestResponse) match {
        case Same(format) => println("%06d: SAME (%s) - %s " format (idx, format, pathAndParams))
        case Different(format, diff) => {
          println("%06d: DIFFERENT (%s) - %s" format (idx, format, pathAndParams))
          writeToFile(new File("result/%06d.diff" format idx), pathAndParams + "\n" + diff)
          writeToFile(new File("result/%06d.master.%s" format (idx, format)), masterResponse)
          writeToFile(new File("result/%06d.lift.%s" format (idx, format)), liftRestResponse)
        }
      }
    } catch {
      case sc: StatusCode => println("Unexpected failure returned %s for %s".format(sc.code, pathAndParams))
    }
  }

  new File("result").mkdirs()

  // Load up the provided log file
//  val logFile = Source.fromFile("logs/200_urls.txt").
//    getLines().
//    zipWithIndex.
//    //drop(4).
//    take(1000)

  val logFile = Source.fromFile("logs/kindle.txt").
    getLines().
    zipWithIndex

  logFile.toList.par.foreach(processline)

  h.shutdown()
}
