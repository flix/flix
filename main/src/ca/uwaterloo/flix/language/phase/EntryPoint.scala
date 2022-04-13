/*
 * Copyright 2022 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Scheme, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, mapN}

/**
  * Processes the entry point of the program.
  *
  * The entry point is replaced by a new function (`main%`) that calls the old entry point (`func`).
  *
  * The argument to the `func` must have type `Unit`.
  *
  * If the result type of `func` has type `Unit`,
  * then the result is returned from `main%` as normal.
  * If the result type of `func` is some other type with a `ToString` instance,
  * then the result is printed in `main%`.
  * If the result type of `func` is some other type without a `ToString` instance,
  * then an error is raised.
  *
  * For example, given an entry point `func` with type `Unit -> Float64`,
  * we produce:
  * {{{
  *  pub def main%(): Unit & Impure = {
  *      println(func(args))
  *  }
  * }}}
  */
object EntryPoint {

  /**
    * The scheme of the entry point function.
    * `Unit -> Unit`
    */
  private val EntryPointScheme = Scheme(Nil, Nil, Type.mkImpureArrow(Type.Unit, Type.Unit, SourceLocation.Unknown))

  /**
    * The default entry point in case none is specified. (`main`)
    */
  private val DefaultEntryPoint = Symbol.mkDefnSym("main")


  /**
    * Introduces a new function `main%` which calls the entry point (if any).
    */
  def run(root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, EntryPointError] = flix.phase("EntryPoint") {
    mapN(findOriginalEntryPoint(root)) {
      // Case 1: We have an entry point. Wrap it.
      case Some(entryPoint0) =>
        val entryPoint = visitEntryPoint(entryPoint0, root)
        root.copy(
          defs = root.defs + (entryPoint.sym -> entryPoint),
          entryPoint = Some(entryPoint.sym)
        )
      // Case 2: No entry point. Don't touch anything.
      case None => root
    }
  }

  /**
    * Finds the entry point in the given `root`.
    */
  private def findOriginalEntryPoint(root: KindedAst.Root)(implicit flix: Flix): Validation[Option[KindedAst.Def], EntryPointError] = {
    root.entryPoint match {
      case None => root.defs.get(DefaultEntryPoint) match {
        case None => None.toSuccess
        case Some(entryPoint) => Some(entryPoint).toSuccess
      }
      case Some(sym) => root.defs.get(sym) match {
        case None => EntryPointError.EntryPointNotFound(sym, getArbitrarySourceLocation(root)).toFailure
        case Some(entryPoint) => Some(entryPoint).toSuccess
      }
    }
  }

  /**
    * Retrieves an arbitrary source location from the root.
    */
  private def getArbitrarySourceLocation(root: KindedAst.Root)(implicit flix: Flix): SourceLocation = {
    root.sources.headOption match {
      // Case 1: Some arbitrary source. Use its location.
      case Some((_, loc)) => loc
      // Case 2: No inputs. Give up and use unknown.
      case None => SourceLocation.Unknown
    }
  }


  /**
    * Checks that the given def is a valid entry point,
    * and returns a new entry point that calls it.
    *
    * The new entry point should be added to the AST.
    */
  private def visitEntryPoint(defn: KindedAst.Def, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Def = {
    // MATT collapse if we keep this
    mkEntryPoint(defn, root)
  }

  /**
    * Builds the new entry point function that calls the old entry point function.
    */
  private def mkEntryPoint(oldEntryPoint: KindedAst.Def, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Def = {

    val argSym = Symbol.freshVarSym("_unit", Ast.BoundBy.FormalParam, SourceLocation.Unknown)
    val loc = oldEntryPoint.sym.loc

    val spec = KindedAst.Spec(
      doc = Ast.Doc(Nil, loc),
      ann = Nil,
      mod = Ast.Modifiers.Empty,
      tparams = Nil,
      fparams = List(KindedAst.FormalParam(argSym, Ast.Modifiers.Empty, Type.Unit, loc)),
      sc = EntryPointScheme,
      tpe = Type.Unit,
      eff = Type.Impure,
      loc = loc
    )

    val func = KindedAst.Expression.Def(oldEntryPoint.sym, Type.freshVar(Kind.Star, loc), loc)

    // func()
    val call = KindedAst.Expression.Apply(func, List(KindedAst.Expression.Unit(loc)), Type.freshVar(Kind.Star, loc), Type.freshVar(Kind.Star, loc), loc)

    // printUnlessUnit(func(args))
    val printSym = root.defs(new Symbol.DefnSym(None, Nil, "printUnlessUnit", loc)).sym
    val printFunc = KindedAst.Expression.Def(printSym, Type.freshVar(Kind.Star, loc), loc)
    val exp = KindedAst.Expression.Apply(printFunc, List(call), Type.freshVar(Kind.Star, loc), Type.freshVar(Kind.Bool, loc), loc)

    val sym = new Symbol.DefnSym(None, Nil, "main" + Flix.Delimiter, loc)

    KindedAst.Def(sym, spec, exp)
  }
}

