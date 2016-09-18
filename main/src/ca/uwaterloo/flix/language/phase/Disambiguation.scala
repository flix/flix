/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.NamedAst.Program
import ca.uwaterloo.flix.language.ast.{Ast, Name, NamedAst}
import ca.uwaterloo.flix.language.errors.TypeError.UnresolvedDefinition
import ca.uwaterloo.flix.language.phase.Unification._

object Disambiguation {

  /**
    * An ADT for the result of looking up a name.
    */
  sealed trait LookupResult

  object LookupResult {

    case class Defn(defn: NamedAst.Declaration.Definition) extends LookupResult

    case class Hook(hook: Ast.Hook) extends LookupResult

  }

  /**
    * Finds the definition with the qualified name `qname` in the namespace `ns0`.
    */
  def lookupRef(qname: Name.QName, ns0: Name.NName, program: Program): InferMonad[LookupResult] = {
    // check whether the reference is fully-qualified.
    if (qname.isUnqualified) {
      // Case 1: Unqualified reference. Try the local namespace.
      val defns = program.definitions.getOrElse(ns0, Map.empty)
      defns.get(qname.ident.name) match {
        case None =>
          // Case 1.1: The definition was not found in the local namespace.
          // Check if it is a hook.
          // TODO: Try to lookup the name as a hook
          failM(UnresolvedDefinition(qname, ns0, qname.loc))
        case Some(defn) =>
          // Case 1.2: The definition was found. Return it.
          liftM(LookupResult.Defn(defn))
      }
    } else {
      // Case 2: Qualified. Lookup the namespace.
      program.definitions.get(qname.namespace) match {
        case None =>
          throw new RuntimeException(s"namespace ${qname.namespace} not found") // TODO: namespace doesnt exist.
        case Some(nm) => nm.get(qname.ident.name) match {
          case None => ??? // TODO: name doesnt exist in namespace.
          case Some(defn) => liftM(LookupResult.Defn(defn))
        }
      }
    }
  }

}
