package com.gu.contentapi.difftool

import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._

object JsonDiff {
  def r(desc: String, json: JValue) = json match {
    case JNothing => ""
    case other => "LINES %s: \n%s\n\n" format (desc, pretty(render(other)))
  }

  def diff(master: String, liftRest: String): DiffResult = {
    val masterJson = parse(master)
    val liftJson = parse(liftRest)

    // remove empty list fields
    //  the "remove" here removes the fields, but they become JNothings in the
    //  AST, so you have to round trip to a string to really remove it. Hmm.
    val cleanLiftJson = parse(compact(render(liftJson remove tidyLiftJson)))
    val cleanMasterJson = parse(compact(render(masterJson remove tidyMasterJson)))

    cleanMasterJson diff cleanLiftJson match {
      case Diff(JNothing, JNothing, JNothing) => Same("json")
      case Diff(changed, added, deleted) =>
        Different("json", r("MODIFIED", changed) + r("ADDED", added) + r("DELETED", deleted))
    }
  }

  def tidyLiftJson(value: JValue) = value match {
    // remove all empty liist fields
    case JField(_, JArray(Nil)) => true
    case _ => false

  }

  def tidyMasterJson(value: JValue) = value match {
    // remove references
    case JField("references", JArray(_)) => true

    // remove mediaAssets "hasStoryPackage"
    //case JField("hasStoryPackage", JString("false")) => true
    //case JField("mediaAssets", JString("false")) => true

    case _ => false

  }

}