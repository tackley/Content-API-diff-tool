package com.gu.contentapi.difftool


sealed trait DiffResult { def format: String }
case class Same(format: String) extends DiffResult
case class Different(format: String, diff: String) extends DiffResult

