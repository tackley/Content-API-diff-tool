package com.gu.contentapi.difftool

import java.io.File
import xml.Elem._
import xml.transform.{RuleTransformer, RewriteRule}
import xml.{XML, Elem, Node}
import OwenDiff.Equal

object XmlDiff extends FileHelpers {
  def cannonicalizeXml(xml: String) = {
    import sys.process._

    xml.replaceAll( """<?xml version='1.0' encoding='utf-8'?>""", "")

    val tmpFile = File.createTempFile("abc", "xml")

    writeToFile(tmpFile, alphabetizeFields(xml).toString())

    val cmd = ("xmllint --c14n " + tmpFile.getCanonicalPath) #| "xmllint --format -"
    cmd.!!
  }

  def alphabetizeFields(xml: String) = {
    object FieldSorter extends RewriteRule {
      override def transform(n: Node) = n match {
        case Elem(prefix, "fields", attribs, scope, children@_*) =>
          val sortedChildren = children.sortBy(_.attribute("name").map(_.text).getOrElse(""))
          Elem(prefix, "fields", attribs, scope, sortedChildren: _*)
        case other => other
      }
    }

    val transfomer = new RuleTransformer(FieldSorter)
    transfomer(XML.loadString(xml))
  }

  def diff(master: String, liftRest: String): DiffResult = {
    val c14nMaster = cannonicalizeXml(master)
    val c14nLift = cannonicalizeXml(liftRest)
    val diffResult = OwenDiff.Diff.diff(c14nMaster.split("\n"), c14nLift.split("\n"))

    val nonEquals = diffResult.flatMap {
      case _: Equal => None
      case other => Some(other)
    }

    if (nonEquals.isEmpty) {
      Same("xml")
    } else {
      Different("xml", nonEquals.mkString)
    }
  }
}