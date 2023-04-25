/*
 * Copyright 2022 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.dbg.AstPrinter
import ca.uwaterloo.flix.language.dbg.AstPrinter.formatLiftedAst

import java.io.{IOException, PrintWriter, StringWriter}
import java.nio.file.{Files, Path}

object CrashHandler {

  /**
    * Creates a crash report for the given exception `ex`.
    */
  def handleCrash(ex: Throwable)(implicit flix: Flix): Unit = {
    // Get the report.
    val report = getCrashReport(ex)

    // Print it.
    println(report)

    // Write it to a file.
    getNextAvailableLogFile() match {
      case None => // Nop
      case Some(path) =>
        try {
          Files.writeString(path, report)
          printAsts()
        } catch {
          case _: IOException =>
            println(s"Unable to write crash report to: '$path'.")
        }
    }
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

  /**
    * Write all asts to the build folder.
    */
  private def printAsts()(implicit flix: Flix): Unit = {
    AstPrinter.writeToDisk("Lifted Ast", formatLiftedAst(flix.getLiftedAst))
  }

}
