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

import ca.uwaterloo.flix.api.effectlock.serialization.{Deserialize, Serialize}
import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, TypedAst}
import ca.uwaterloo.flix.api.effectlock.UseGraph.UsedSym
import ca.uwaterloo.flix.language.ast.shared.{Origin, Source}
import ca.uwaterloo.flix.util.Result

object EffectLock {

  /**
    * Deserializes `json` to a collection of schemes pointed to by either a def or sig.
    */
  def deserialize(json: String): Result[(Map[Symbol.DefnSym, Scheme], Map[Symbol.SigSym, Scheme]), String] = {
    try {
      implicit val formats: org.json4s.Formats = serialization.formats
      val serializableAST = org.json4s.native.Serialization.read[Map[String, serialization.DefOrSig]](json)
      val sdefs = serializableAST.collect {
        case (_, defn: serialization.SDef) => defn
      }
      val ssigs = serializableAST.collect {
        case (_, sig: serialization.SSig) => sig
      }
      val defs = sdefs.map(Deserialize.deserializeDef).toMap
      val sigs = ssigs.map(Deserialize.deserializeSig).toMap
      Result.Ok((defs, sigs))
    } catch {
      case e: Exception => Result.Err(s"Unexpected JSON: ${e.getMessage}")
    }
  }

  /**
    * Serializes the relevant functions  for effect locking in `root` and returns a JSON string.
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
      isFromLocalProject(getSource(src)) &&
        isLibraryFunction(dst.src) &&
        root.defs.get(dst).exists(_.spec.mod.isPublic)

    case (src, UsedSym.SigSym(dst)) =>
      isFromLocalProject(getSource(src)) &&
        isLibraryFunction(dst.src) &&
        root.sigs.get(dst).exists(_.spec.mod.isPublic)
  }

  /** Returns the source of `sym0`. This is a helper function to reduce repetition. */
  private def getSource(sym0: UsedSym): Source = sym0 match {
    case UsedSym.DefnSym(sym) => sym.src
    case UsedSym.SigSym(sym) => sym.src
  }

  /** Returns `true` if `src` is in the source project, i.e. was supplied by the user. */
  private def isFromLocalProject(src: Source): Boolean = src.origin.isUser

  /**
    * Returns `true` if `src` is in a library: the library bundled with the compiler or a package.
    *
    * A synthetic source with an unknown origin is in no library.
    */
  private def isLibraryFunction(src: Source): Boolean = src.origin match {
    case Origin.User => false
    case Origin.Library => true
    case Origin.Package => true
    case Origin.Unknown => false
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
