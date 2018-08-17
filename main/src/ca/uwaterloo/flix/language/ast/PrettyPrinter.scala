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
  def fmt(f: FinalAst.Constraint, sb: StringBuilder): StringBuilder = f.head match {
    case FinalAst.Predicate.Head.True(loc) => sb.append("true")
    case FinalAst.Predicate.Head.False(loc) => sb.append("false")
    case FinalAst.Predicate.Head.RelAtom(baseOpt, sym, terms, loc) =>
      sb.append(sym).append("(").append(terms.map(t => fmt(t, new StringBuilder)).mkString(", ")).append(").")
    case FinalAst.Predicate.Head.LatAtom(baseOpt, sym, terms, loc) =>
      sb.append(sym).append("(").append(terms.map(t => fmt(t, new StringBuilder)).mkString(", ")).append(").")
  }

  /**
    * Returns a string representation of the given term.
    */
  def fmt(t: FinalAst.Term.Head, sb: StringBuilder): StringBuilder = t match {
    case FinalAst.Term.Head.QuantVar(sym, tpe, loc) => sb.append(sym.toString)
    case FinalAst.Term.Head.CapturedVar(sym, tpe, loc) => sb.append(sym.toString)
    case FinalAst.Term.Head.Lit(sym, tpe, loc) => sb.append(sym.toString)
    case FinalAst.Term.Head.App(name, args, tpe, loc) => ???
  }

}
