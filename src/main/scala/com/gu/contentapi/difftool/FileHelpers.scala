package com.gu.contentapi.difftool

import java.io.{FileWriter, File}

trait FileHelpers {
  def writeToFile(file: File, s: String) {
    val writer = new FileWriter(file)
    writer.write(s)
    writer.close()
  }


}