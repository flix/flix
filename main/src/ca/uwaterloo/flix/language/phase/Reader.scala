/*
 * Copyright 2017 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.{AvailableClasses, Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.ast.{ReadAst, SourceLocation}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, StreamOps}

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.util.Using

/**
  * A phase to read inputs into memory.
  */
object Reader {

  /**
    * Reads the given source inputs into memory.
    */
  def run(inputs: List[Input], availableClasses: AvailableClasses)(implicit flix: Flix): (ReadAst.Root, List[CompilationMessage]) =
    flix.phase("Reader") {

      val result = mutable.Map.empty[Source, Unit]
      for (input <- inputs) {
        input match {
          case Input.RealFile(path, _) =>
            val bytes = Files.readAllBytes(path)
            val str = new String(bytes, flix.defaultCharset)
            val src = Source.fromString(input, str)
            result += (src -> ())

          case Input.VirtualFile(_, text, _) =>
            val src = Source.fromString(input, text)
            result += (src -> ())

          case Input.VirtualUri(_, text, _) =>
            val src = Source.fromString(input, text)
            result += (src -> ())

          case Input.PkgFile(path, sctx) =>
            for (src <- unpack(path)(sctx, flix)) {
              result += (src -> ())
            }

          case Input.FileInPackage(_, _, _, _) => throw InternalCompilerException("Impossible.", SourceLocation.Unknown)

          case Input.Unknown => throw InternalCompilerException("Impossible.", SourceLocation.Unknown)
        }
      }

      val sources = result.toMap
      (ReadAst.Root(sources, availableClasses), List.empty)
    }(DebugNoOp())

  /**
    * Returns a list of sources extracted from the given flix package at path `p`.
    */
  private def unpack(p: Path)(implicit sctx: SecurityContext, flix: Flix): List[Source] = {
    // Check that the path is a flix package.
    flix.isValidFpkgFile(p) match {
      case Result.Err(_) => throw new RuntimeException(s"The path '$p' is not a flix package.")
      case Result.Ok(()) =>
        // Open the zip file.
        Using(new ZipFile(p.toFile)) { zip =>
          // Collect all source and test files.
          val result = mutable.ArrayBuffer.empty[Source]
          val iterator = zip.entries()
          while (iterator.hasMoreElements) {
            val entry = iterator.nextElement()
            val name = entry.getName
            if (name.endsWith(".flix")) {
              val virtualPath = p.getFileName.toString + ":" + name
              val bytes = StreamOps.readAllBytes(zip.getInputStream(entry))
              val str = new String(bytes, flix.defaultCharset)
              val input = Input.FileInPackage(p, virtualPath, str, sctx)
              result += Source.fromString(input, str)
            }
          }
          result.toList
        }.get // TODO Return a Result instead, see https://github.com/flix/flix/issues/3132
    }
  }

}
