/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

// TODO: Deprecated.
object PrettyPrinter {

  /**
    * Returns a string representation of the given constraint.
    */
  def fmt(f: ExecutableAst.Constraint, sb: StringBuilder): StringBuilder = f.head match {
    case ExecutableAst.Predicate.Head.True(loc) => sb.append("true")
    case ExecutableAst.Predicate.Head.False(loc) => sb.append("false")
    case ExecutableAst.Predicate.Head.RelAtom(sym, terms, loc) =>
      sb.append(sym).append("(").append(terms.map(t => fmt(t, new StringBuilder)).mkString(", ")).append(").")
    case ExecutableAst.Predicate.Head.LatAtom(sym, terms, loc) =>
      sb.append(sym).append("(").append(terms.map(t => fmt(t, new StringBuilder)).mkString(", ")).append(").")
  }

  /**
    * Returns a string representation of the given term.
    */
  def fmt(t: ExecutableAst.Term.Head, sb: StringBuilder): StringBuilder = t match {
    case ExecutableAst.Term.Head.Var(sym, tpe, loc) => sb.append(sym.toString)
    case ExecutableAst.Term.Head.Lit(lit, tpe, loc) => sb.append(lit.toString)
    case ExecutableAst.Term.Head.Cst(sym, tpe, loc) => sb.append(sym.toString)
    case ExecutableAst.Term.Head.App(name, args, tpe, loc) => ???
  }

}
