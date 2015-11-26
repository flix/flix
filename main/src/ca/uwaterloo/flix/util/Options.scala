package ca.uwaterloo.flix.util

object Options {
  val Default = Options(
    debugger = Debugger.Disabled
  )
}

case class Options(debugger: Debugger)

sealed trait Debugger

object Debugger {

  case object Enabled extends Debugger

  case object Disabled extends Debugger

}
