package ca.uwaterloo.flix

import java.nio.file.{Path, InvalidPathException, Files, Paths}

import ca.uwaterloo.flix.util.Validation

/**
 * The main entry point for the Flix compiler and runtime.
 */
object Main {

  /**
   * The main method.
   */
  def main(args: Array[String]): Unit = {
    val paths = args flatMap getValidPath

    Flix.fromPaths(paths: _*) match {
      case Validation.Success(model, errors) =>
        errors.foreach(e => println(e.format))
        model.print()
      case Validation.Failure(errors) =>
        errors.foreach(e => println(e.format))
    }
  }

  /**
   * Optionally returns the given path `s` if it is a valid path.
   */
  private def getValidPath(s: String): Option[Path] = try {
    val path = Paths.get(s)
    if (!Files.exists(path) || !Files.isRegularFile(path))
      None
    else
      Some(path)
  } catch {
    case e: InvalidPathException => None
  }

}
