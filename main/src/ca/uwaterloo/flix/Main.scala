package ca.uwaterloo.flix

import java.nio.file.{Path, InvalidPathException, Files, Paths}

import ca.uwaterloo.flix.util.{Debugger, Options, Validation}

/**
 * The main entry point for the Flix compiler and runtime.
 */
object Main {

  /**
   * The main method.
   */
  def main(args: Array[String]): Unit = {

    // TODO: Commandline arguments.

    val t = System.nanoTime()
    val paths = args flatMap getValidPath

    val debugger = args.exists(a => a == "-d" || a == "--debugger")

    var options = Options.Default
    if (debugger)
      options = options.copy(debugger = Debugger.Enabled)

    Flix.mkPath(paths, options) match {
      case Validation.Success(model, errors) =>
        errors.foreach(e => println(e.format))
        model.print()
      case Validation.Failure(errors) =>
        errors.foreach(e => println(e.format))
    }

    val e = (System.nanoTime() - t) / 1000000
    Console.println(f"Total execution time: $e%,d msec.")
  }

  /**
   * Optionally returns the given path `s` if it is a valid path.
   */
  private def getValidPath(s: String): Option[Path] = try {
    val path = Paths.get(s)
    if (!Files.exists(path) || !Files.isRegularFile(path)) {
      Console.println(s"Skipping $s. Not a valid path.")
      None
    }
    else
      Some(path)
  } catch {
    case e: InvalidPathException => None
  }

}
