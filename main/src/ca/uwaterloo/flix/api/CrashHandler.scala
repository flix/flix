/*
 * Copyright 2022 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api

import java.io.{IOException, PrintWriter, StringWriter}
import java.nio.file.{Files, Path}

object CrashHandler {

  /**
    * Creates a crash report for the given exception `ex`, and returns its absolute path if successful.
    */
  def handleCrash(ex: Throwable)(implicit flix: Flix): Option[Path] = {
    // Get the report.
    val report = getCrashReport(ex)

    // Print it.
    println(report)

    // Write it to a file.
    val optPath =
      getNextAvailableLogFile() match {
        case None => None
        case Some(path) =>
          try {
            Files.writeString(path, report)
            Some(path)
          } catch {
            case _: IOException =>
              println(s"Unable to write crash report to: '$path'.")
              None
          }
      }

    optPath.map(_.toAbsolutePath.normalize)
  }

  /**
    * Returns an error message with auxiliary information for the given exception `ex`.
    */
  private def getCrashReport(ex: Throwable)(implicit flix: Flix): String = {
    val message = ex.getMessage
    val flixVersion = Version.CurrentVersion.toString
    val jvmVersion = System.getProperty("java.version")
    val jvmDate = System.getProperty("java.version.date")
    val jvmVendor = System.getProperty("java.vendor")
    val javaHome = System.getProperty("java.home")
    val osName = System.getProperty("os.name")
    val osVersion = System.getProperty("os.version")
    s"""#
       |# An unexpected error has been detected by the Flix compiler:
       |#
       |#   $message
       |#
       |# This is a bug in the Flix compiler. Please report it here:
       |#
       |# https://github.com/flix/flix/issues
       |#
       |# -- Flix Compiler --
       |#
       |# Flix Version : $flixVersion
       |#   incremental: ${flix.options.lib}
       |#
       |# -- Java Virtual Machine --
       |#
       |# JVM Version  : $jvmVersion ($jvmDate)
       |# JVM Vendor   : $jvmVendor
       |# JAVA_HOME    : $javaHome
       |# System       : $osName ($osVersion)
       |#
       |# -- Stack Trace --
       |${getStackTrace(ex)}
       |""".stripMargin
  }

  /**
    * Returns the stack trace of the given exception `ex` as a string.
    */
  private def getStackTrace(ex: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    ex.printStackTrace(pw)
    sw.toString
  }

  /**
    * Returns the next available logfile name.
    */
  private def getNextAvailableLogFile(): Option[Path] = {
    for (i <- 0 until 100) {
      val p = Path.of("./" + "crash_report_" + i + ".txt")
      if (!Files.exists(p)) {
        return Some(p)
      }
    }
    None
  }

}
