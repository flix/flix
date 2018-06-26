package ca.uwaterloo.flix.runtime.solver

import java.time.Duration

class FixpointOptions {

  private var monitored: Boolean = false

  private var threads: Int = 1

  private var timeout: Option[Duration] = None

  private var verbose: Boolean = false

  def isMonitored: Boolean = monitored

  def setMonitored(v: Boolean): Unit = {
    monitored = v
  }

  def getThreads: Int = threads

  def setThreads(v: Int): Unit = {
    threads = v
  }

  def getTimeout: Option[Duration] = timeout

  def setTimeout(v: Option[Duration]): Unit = {
    timeout = v
  }

  def isVerbose: Boolean = verbose

  def setVerbose(v: Boolean): Unit = {
    verbose = v
  }

}
