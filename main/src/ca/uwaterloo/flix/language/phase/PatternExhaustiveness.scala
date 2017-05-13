/*
 * Copyright 2017 Jason Mittertreiner
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The Pattern Exhaustiveness phase checks pattern matches for exhaustiveness
  * as well as for useless patterns
  *
  * A pattern is useless if:
  * A pattern match is exhaustive if:
  */
object PatternExhaustiveness extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Returns an error message if a pattern match is not exhaustive
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = {
    if (flix.options.debug) {
      // Can probably do options stuff here
    }
    implicit val _ = flix.genSym
    val defns = root.definitions.map { case (k, v) => k -> Definition.CheckPats(v) }

    root.toSuccess
  }

  object Definition {
    def CheckPats(tast: TypedAst.Declaration.BoundedLattice)(implicit genSym: GenSym): Validation[TypedAst.Declaration.BoundedLattice, CompilationError] = tast match {
      case TypedAst.Declaration.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) =>
        for {
          _bot <- Expression.CheckPats(bot)
          _top <- Expression.CheckPats(top)
          _leq <- Expression.CheckPats(leq)
          _lub <- Expression.CheckPats(lub)
          _glb <- Expression.CheckPats(glb)
        } yield TypedAst.Declaration.BoundedLattice(tpe, _bot, _top, _leq, _lub, _glb, loc)
    }
    def CheckPats(tast: TypedAst.Declaration.Definition)(implicit genSym: GenSym): TypedAst.Declaration.Definition = tast

    def CheckPats(tast: TypedAst.Declaration.Index)(implicit genSym: GenSym): TypedAst.Declaration.Index = tast

  }

  object Expression {
    def CheckPats(tast: TypedAst.Expression)(implicit genSym: GenSym): Validation[TypedAst.Expression, CompilationError] = tast match {
      case TypedAst.Expression.Match(exp0, rules, tpe, loc) => Success(tast, Stream.empty)
      case a => Success(a, Stream.empty)
    }
  }
}
