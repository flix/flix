package ca.uwaterloo.flix.util

class StopWatch {

  private var timestamp = System.nanoTime()

  def click(): Long = {
    val current = System.nanoTime()
    val elapsed = current - timestamp
    timestamp = current
    elapsed
  }

}
