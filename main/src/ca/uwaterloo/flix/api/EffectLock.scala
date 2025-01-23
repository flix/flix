package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.util.Validation

import java.nio.file.{Files, Path}

object EffectLock {

  /**
    * Returns the path to the Manifest file relative to the given path `p`.
    */
  private def getEffectLockFile(p: Path): Path = p.resolve("./effect.lock").normalize()


  def effectLock(path: Path, bootstrap: Bootstrap, flix: Flix): Validation[Unit, BootstrapError] = {
    bootstrap.check(flix) match {
      case Validation.Failure(errors) => Validation.Failure(errors)
      case Validation.Success(root) =>
        val outputStream = Files.newOutputStream(getEffectLockFile(path))
        val signatures = mkEffectLockSignatures(root).mkString("\n") // TODO: make JSON
        Validation.Success(outputStream.write(signatures.getBytes))
    }
  }

  def mkEffectLockSignatures(root: TypedAst.Root): List[String] = {
    root.defs.map {
      // TODO: Consider types / type schemes and only reachable functions / visible functions
      case (sym, defn) => s"$sym:${defn.spec}"
    }.toList
  }
}
