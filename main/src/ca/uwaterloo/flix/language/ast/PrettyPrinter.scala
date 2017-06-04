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

object PrettyPrinter {

  /**
    * Returns a string representation of the given expression.
    */
  def fmt(e: ExecutableAst.Expression, sb: StringBuilder): StringBuilder = e match {
    case ExecutableAst.Expression.Unit => sb.append("()")
    case ExecutableAst.Expression.True => sb.append("true")
    case ExecutableAst.Expression.False => sb.append("false")
    case ExecutableAst.Expression.Char(c) => sb.append("'").append(c).append("'")
    case ExecutableAst.Expression.Float32(f) => sb.append(f).append("f32")
    case ExecutableAst.Expression.Float64(f) => sb.append(f).append("f64")
    case ExecutableAst.Expression.Int8(i) => sb.append(i.toString).append("i8")
    case ExecutableAst.Expression.Int16(i) => sb.append(i.toString).append("i16")
    case ExecutableAst.Expression.Int32(i) => sb.append(i.toString).append("i32")
    case ExecutableAst.Expression.Int64(i) => sb.append(i.toString).append("i64")
    case ExecutableAst.Expression.BigInt(i) => sb.append(i.toString).append("ii")
    case ExecutableAst.Expression.Str(s) => sb.append("\"").append(s).append("\"")
    case _ => ???
  }

  /**
    * Returns a string representation of the given constraint.
    */
  def fmt(f: ExecutableAst.Constraint, sb: StringBuilder): StringBuilder = f.head match {
    case ExecutableAst.Predicate.Head.True(loc) => sb.append("true")
    case ExecutableAst.Predicate.Head.False(loc) => sb.append("false")
    case ExecutableAst.Predicate.Head.Positive(sym, terms, loc) =>
      sb.append(sym).append("(").append(terms.map(t => fmt(t, new StringBuilder)).mkString(", ")).append(").")
    case ExecutableAst.Predicate.Head.Negative(sym, terms, loc) =>
      sb.append("!").append(sym).append("(").append(terms.map(t => fmt(t, new StringBuilder)).mkString(", ")).append(").")
  }

  /**
    * Returns a string representation of the given term.
    */
  def fmt(t: ExecutableAst.Term.Head, sb: StringBuilder): StringBuilder = t match {
    case ExecutableAst.Term.Head.Var(sym, tpe, loc) => sb.append(sym.toString)
    case ExecutableAst.Term.Head.Lit(sym, tpe, loc) => sb.append(sym.toString)
    case ExecutableAst.Term.Head.App(name, args, tpe, loc) => ???
  }

}
