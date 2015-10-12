package ca.uwaterloo.flix

import java.nio.file.{InvalidPathException, Files, Paths}

import ca.uwaterloo.flix.language.Compiler

object Flix {

  def solve(s: String): Unit = {
    if (isValidPath(s))
      Compiler.compile(Paths.get(s))
    else
      Compiler.compile(s)
  }

  private def isValidPath(s: String): Boolean = try {
    val path = Paths.get(s)
    Files.exists(path) && Files.isRegularFile(path)
  } catch {
    case e: InvalidPathException => false
  }

}
