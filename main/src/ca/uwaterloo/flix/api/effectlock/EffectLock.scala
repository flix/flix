/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.effectlock.UseGraph.UsedSym
import ca.uwaterloo.flix.api.effectlock.serialization.Serialize
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.Input
import ca.uwaterloo.flix.util.Result

object EffectLock {

  /**
    * Serializes the relevant functions for effect locking in `root` and returns a JSON string.
    * If it returns `Ok(json)`, then `json` may be written directly to a file.
    */
  def lock(root: TypedAst.Root): Result[String, String] = {
    try {
      val serializableAST = mkSerialization(root)
      val typeHints = serialization.formats
      val res = org.json4s.native.Serialization.write(serializableAST)(typeHints)
      Result.Ok(res)
    } catch {
      case e: Exception => Result.Err(s"Invalid AST: ${e.getMessage}")
    }
  }

  /**
    * Returns a map of defs and signatures in `root` that must be effect locked.
    * The map may directly be converted to a string using [[serialization.formats]] for type hints.
    */
  private def mkSerialization(root: TypedAst.Root): Map[String, serialization.DefOrSig] = {
    val useGraph = UseGraph.computeGraph(root).filter(isPublicLibraryCall(_, root)).map { case (_, libDefn) => libDefn }
    val defs = useGraph.flatMap(getLibraryDefn(_, root)).toMap
    val defSerialization = defs.map { case (sym, defn) => sym.toString -> Serialize.serializeDef(defn) }
    val sigs = useGraph.flatMap(getLibrarySig(_, root)).toMap
    val sigSerialization = sigs.map { case (sym, sig) => sym.toString -> Serialize.serializeSig(sig) }
    defSerialization ++ sigSerialization
  }

  /** Returns `true` if for the edge `f -> g`, `f` occurs in the source project and `g` occurs in a library and `g` is public. */
  private def isPublicLibraryCall(graphEdge: (UsedSym, UsedSym), root: TypedAst.Root): Boolean = graphEdge match {
    case (src, UsedSym.DefnSym(dst)) =>
      isFromLocalProject(getInput(src)) &&
        isLibraryFunction(dst.src.input) &&
        root.defs.get(dst).exists(_.spec.mod.isPublic)

    case (src, UsedSym.SigSym(dst)) =>
      isFromLocalProject(getInput(src)) &&
        isLibraryFunction(dst.src.input) &&
        root.sigs.get(dst).exists(_.spec.mod.isPublic)
  }

  /** Returns the input source of `sym0`. This is a helper function to reduce repetition. */
  private def getInput(sym0: UsedSym): Input = sym0 match {
    case UsedSym.DefnSym(sym) => sym.src.input
    case UsedSym.SigSym(sym) => sym.src.input
  }

  /** Returns `true` if `input` is in the source project. */
  private def isFromLocalProject(input: Input): Boolean = input match {
    case Input.RealFile(_, _) => true
    case Input.VirtualFile(_, _, _) => true
    case Input.VirtualUri(_, _, _) => true
    case Input.PkgFile(_, _) => false
    case Input.FileInPackage(_, _, _, _) => false
    case Input.Unknown => false
  }

  /** Returns `true` if `input` is in a library. */
  private def isLibraryFunction(input: Input): Boolean = input match {
    case Input.RealFile(_, _) => false
    case Input.VirtualFile(_, _, _) => false
    case Input.VirtualUri(_, _, _) => false
    case Input.PkgFile(_, _) => true
    case Input.FileInPackage(_, _, _, _) => true
    case Input.Unknown => false
  }

  /** Returns the definition of `sym0` w.r.t. `root`. */
  private def getLibraryDefn(sym0: UsedSym, root: TypedAst.Root): Option[(Symbol.DefnSym, TypedAst.Def)] = sym0 match {
    case UsedSym.DefnSym(sym) =>
      Some(sym -> root.defs(sym))

    case UsedSym.SigSym(_) =>
      None
  }

  /** Returns the definition of `sym0` w.r.t. `root`. */
  private def getLibrarySig(graphEdge: UsedSym, root: TypedAst.Root): Option[(Symbol.SigSym, TypedAst.Sig)] = graphEdge match {
    case UsedSym.DefnSym(_) =>
      None

    case UsedSym.SigSym(sym) =>
      Some(sym -> root.sigs(sym))
  }
}
