package ca.uwaterloo.flix.util

object Options {
  /**
    * Default options.
    */
  val Default = Options(
    debugger = Debugger.Disabled,
    verbosity = Verbosity.Normal,
    verify = Verify.Disabled
  )
}

/**
  * General Flix options.
  *
  * @param debugger  enable or disable the built-in web-based debugger.
  * @param verbosity the level of verbosity.
  * @param verify    enable or disable the built-in verifier.
  */
case class Options(debugger: Debugger, verbosity: Verbosity, verify: Verify)


/**
  * An option to enable or disable the built-in web-based debugger.
  *
  * Note: Enabling the debugger may incur a significant performance overhead.
  */
sealed trait Debugger

object Debugger {

  /**
    * Enables the built-in web-based debugger.
    */
  case object Enabled extends Debugger

  /**
    * Disables the built-in web-based debugger.
    */
  case object Disabled extends Debugger

}

/**
  * An option to control the level of verbosity.
  */
sealed trait Verbosity

object Verbosity {

  /**
    * Output verbose information. Useful for debugging.
    */
  case object Verbose extends Verbosity

  /**
    * Output condensed information. The default.
    */
  case object Normal extends Verbosity

  /**
    * Output nothing. Useful for when Flix is used as a library.
    */
  case object Silent extends Verbosity

}

/**
  * An option to control whether verification is enabled.
  */
sealed trait Verify

object Verify {

  /**
    * Enables the built-in verifier.
    */
  case object Enabled extends Verify

  /**
    * Disables the built-in verifier.
    */
  case object Disabled extends Verify

}
