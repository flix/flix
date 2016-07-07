/*
 * Copyright 2015-2016 Magnus Madsen
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

import scala.collection.immutable.Seq

/**
  * ParsedAst super-type.
  */
sealed trait ParsedAst

object ParsedAst {

  /**
    * Program. A collection of abstract syntax trees.
    *
    * @param roots the roots of the abstract syntax trees in the program.
    * @param time  the time spent in each compiler phase.
    */
  case class Program(roots: List[ParsedAst.Root], time: Time) extends ParsedAst

  /**
    * Root. A collection of imports and declarations.
    *
    * @param imports the imports declared in the abstract syntax tree.
    * @param decls   the declarations in the abstract syntax tree.
    */
  case class Root(imports: Seq[ParsedAst.Import], decls: Seq[ParsedAst.Declaration]) extends ParsedAst

  /**
    * Imports.
    */
  sealed trait Import extends ParsedAst

  object Import {

    /**
      * Import Wildcard.
      *
      * E.g. `import foo.bar.baz/_`.
      *
      * @param sp1 the position of the first character in the import.
      * @param ns  the name of the namespace.
      * @param sp2 the position of the last character in the import.
      */
    case class Wild(sp1: SourcePosition, ns: Name.NName, sp2: SourcePosition) extends ParsedAst.Import

    /**
      * Import Definition.
      *
      * E.g. `import foo.bar.baz/qux)`.
      *
      * @param sp1  the position of the first character in the import.
      * @param ns   the name of the namespace.
      * @param name the name of the definition.
      * @param sp2  the position of the last character in the import.
      */
    case class Definition(sp1: SourcePosition, ns: Name.NName, name: Name.Ident, sp2: SourcePosition) extends ParsedAst.Import

    /**
      * Import Namespace.
      *
      * E.g. `import foo.bar.baz`.
      *
      * @param sp1 the position of the first character in the import.
      * @param ns  the name of the namespace.
      * @param sp2 the position of the last character in the import.
      */
    case class Namespace(sp1: SourcePosition, ns: Name.NName, sp2: SourcePosition) extends ParsedAst.Import

  }

  /**
    * Declarations.
    */
  sealed trait Declaration extends ParsedAst

  object Declaration {

    /**
      * Namespace Declaration.
      *
      * @param sp1   the position of the first character in the declaration.
      * @param name  the name of the namespace.
      * @param decls the nested declarations.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Namespace(sp1: SourcePosition, name: Name.NName, decls: Seq[ParsedAst.Declaration], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Definition Declaration (top-level function or expression).
      *
      * @param ann       the associated annotations.
      * @param sp1       the position of the first character in the declaration.
      * @param ident     the name of the definition.
      * @param paramsOpt the formal parameters.
      * @param tpe       the declared type.
      * @param exp       the expression.
      * @param sp2       the position of the last character in the declaration.
      */
    case class Definition(ann: Seq[ParsedAst.Annotation], sp1: SourcePosition, ident: Name.Ident, paramsOpt: Option[Seq[Ast.FormalParam]], tpe: Type, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Signature Declaration (top-level function or expression signature).
      *
      * @param sp1       the position of the first character in the declaration.
      * @param ident     the name of the signature.
      * @param paramsOpt the formal parameters.
      * @param tpe       the declared type.
      * @param sp2       the position of the last character in the declaration.
      */
    case class Signature(sp1: SourcePosition, ident: Name.Ident, paramsOpt: Option[Seq[Ast.FormalParam]], tpe: Type, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * External Declaration (external top-level function or expression)
      *
      * @param sp1       the position of the first character in the declaration.
      * @param ident     the name of the external.
      * @param paramsOpt the formal parameters.
      * @param tpe       the declared type.
      * @param sp2       the position of the last character in the declaration.
      */
    case class External(sp1: SourcePosition, ident: Name.Ident, paramsOpt: Option[Seq[Ast.FormalParam]], tpe: Type, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Law Declaration.
      *
      * @param sp1       the position of the first character in the declaration.
      * @param ident     the name of the law.
      * @param tparams   the type parameters.
      * @param paramsOpt the value parameters.
      * @param tpe       the declared type.
      * @param exp       the expression.
      * @param sp2       the position of the last character in the declaration.
      */
    case class Law(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], paramsOpt: Option[Seq[Ast.FormalParam]], tpe: Type, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Enum Declaration.
      *
      * @param sp1   the position of the first character in the declaration.
      * @param ident the name of the enum.
      * @param cases the cases of the enum.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Enum(sp1: SourcePosition, ident: Name.Ident, cases: Seq[ParsedAst.Case], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Class Declaration (type class signature).
      *
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the type class.
      * @param tparams the type parameters of the type class.
      * @param bounds  the context bounds (i.e. type parameter constraints).
      * @param sp2     the position of the last character in the declaration.
      */
    case class Class(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[Type], bounds: Seq[ContextBound], decls: Seq[ParsedAst.Declaration], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Implementation Declaration (type class instance).
      *
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the type class.
      * @param tparams the type parameters of the type class.
      * @param bounds  the context bounds (i.e. type parameter constraints).
      * @param sp2     the position of the last character in the declaration.
      */
    case class Impl(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[Type], bounds: Seq[ContextBound], decls: Seq[ParsedAst.Declaration.Definition], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Relation Declaration.
      *
      * @param sp1   the position of the first character in the declaration.
      * @param ident the name of the relation.
      * @param attr  the attributes (columns) of the relation.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Relation(sp1: SourcePosition, ident: Name.Ident, attr: Seq[Ast.Attribute], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Lattice Declaration.
      *
      * @param sp1   the position of the first character in the declaration.
      * @param ident the name of the lattice.
      * @param attr  the attributes (columns) of the relation.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Lattice(sp1: SourcePosition, ident: Name.Ident, attr: Seq[Ast.Attribute], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Index Declaration.
      *
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the relation or lattice.
      * @param indexes the sequence of indexes.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Index(sp1: SourcePosition, ident: Name.Ident, indexes: Seq[Seq[Name.Ident]], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Fact Declaration.
      *
      * @param sp1  the position of the first character in the declaration.
      * @param head the head predicate.
      * @param sp2  the position of the last character in the declaration.
      */
    case class Fact(sp1: SourcePosition, head: ParsedAst.Predicate, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Rule Declaration.
      *
      * @param sp1  the position of the first character in the declaration.
      * @param head the head predicate.
      * @param body the body predicates.
      * @param sp2  the position of the last character in the declaration.
      */
    case class Rule(sp1: SourcePosition, head: ParsedAst.Predicate, body: Seq[ParsedAst.Predicate], sp2: SourcePosition) extends ParsedAst.Declaration

    @deprecated("Will be replaced by type classes", "0.1.0")
    case class BoundedLattice(sp1: SourcePosition, tpe: Type, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Declaration

  }


  /**
    * Literals.
    */
  sealed trait Literal

  object Literal {

    /**
      * Unit Literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Unit(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * True Literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class True(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * False Literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class False(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Char Literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the char literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Char(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Float32 Literal (32-bit floating-point number).
      *
      * @param sp1    the position of the first character in the literal.
      * @param sign   the sign (true if signed).
      * @param before the digits before the decimal point.
      * @param after  the digits after the decimal point.
      * @param sp2    the position of the last character in the literal.
      */
    case class Float32(sp1: SourcePosition, sign: Boolean, before: String, after: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Float64 Literal (64-bit floating-point number).
      *
      * @param sp1    the position of the first character in the literal.
      * @param sign   the sign (true if signed).
      * @param before the digits before the decimal point.
      * @param after  the digits after the decimal point.
      * @param sp2    the position of the last character in the literal.
      */
    case class Float64(sp1: SourcePosition, sign: Boolean, before: String, after: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int8 Literal (signed 8-bit integer).
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the int8 literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Int8(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int16 Literal (signed 16-bit integer).
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the int16 literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Int16(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int32 Literal (signed 32-bit integer).
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the int32 literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Int32(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int64 Literal (signed 64-bit integer).
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the int64 literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Int64(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * BigInt Literal (arbitrary sized integer).
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the big int literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class BigInt(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * String Literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the string literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Str(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

  }


  /**
    * Expressions.
    */
  sealed trait Expression extends ParsedAst

  object Expression {

    /**
      * Wildcard Expression.
      *
      * Illegal in proper expressions, but allowed in predicates.
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Wild(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Variable Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param name the ambiguous name.
      * @param sp2  the position of the last character in the expression.
      */
    case class Var(sp1: SourcePosition, name: Name.QName, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Literal Expression.
      *
      * Inlined by the Weeder.
      *
      * @param sp1 the position of the first character in the expression.
      * @param lit the literal.
      * @param sp2 the position of the last character in the expression.
      */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Apply Expression (function call).
      *
      * @param lambda the lambda expression.
      * @param args   the arguments.
      * @param sp2    the position of the last character in the expression.
      */
    case class Apply(lambda: ParsedAst.Expression, args: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Infix Expression (function call).
      *
      * Replaced with Apply by Weeder.
      *
      * @param e1   the first argument expression.
      * @param name the name of the function.
      * @param e2   the second argument expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Infix(e1: ParsedAst.Expression, name: Name.QName, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Lambda Expression.
      *
      * @param sp1    the position of the first character in the expression.
      * @param params the formal parameters.
      * @param exp    the body expression.
      * @param sp2    the position of the last character in the expression.
      */
    case class Lambda(sp1: SourcePosition, params: Seq[Name.Ident], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Unary Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param op  the unary operator.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Unary(sp1: SourcePosition, op: UnaryOperator, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Binary Expression.
      *
      * @param exp1 the left expression.
      * @param op   the binary operator.
      * @param exp2 the right expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Binary(exp1: ParsedAst.Expression, op: BinaryOperator, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Extended Binary Expression.
      *
      * @param exp1 the left expression.
      * @param op   the extended binary operator.
      * @param exp2 the right expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class ExtendedBinary(exp1: ParsedAst.Expression, op: ExtBinaryOperator, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * If-then-else Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param exp1 the conditional expression.
      * @param exp2 the consequence expression.
      * @param exp3 the alternative expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class IfThenElse(sp1: SourcePosition, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, exp3: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * LetMatch Expression (let-binding with pattern match).
      *
      * @param sp1  the position of the first character in the expression.
      * @param pat  the match pattern.
      * @param exp1 the value expression.
      * @param exp2 the body expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class LetMatch(sp1: SourcePosition, pat: ParsedAst.Pattern, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Match Expression (pattern match expression).
      *
      * @param sp1   the position of the first character in the expression.
      * @param exp   the value expression.
      * @param rules the match rules and their bodies.
      * @param sp2   the position of the last character in the expression.
      */
    case class Match(sp1: SourcePosition, exp: ParsedAst.Expression, rules: Seq[(ParsedAst.Pattern, ParsedAst.Expression)], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Switch Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param rules the rules of the switch.
      * @param sp2   the position of the last character in the expression.
      */
    case class Switch(sp1: SourcePosition, rules: Seq[(ParsedAst.Expression, ParsedAst.Expression)], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Tag Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param enum the enum name.
      * @param tag  the tag name.
      * @param exp  the optional value expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Tag(sp1: SourcePosition, enum: Name.QName, tag: Name.Ident, exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Tuple Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the expression.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * None Expression (None is value of type Opt[A]).
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class FNone(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Some Expression (Some(v) is value of type Opt[A]).
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the value expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class FSome(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Nil Expression (Nil is the empty list of type List[A]).
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class FNil(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * List Expression (cons cell).
      *
      * @param hd  the head of the list.
      * @param tl  the tail of the list.
      * @param sp2 the position of the last character in the expression.
      */
    case class FList(hd: ParsedAst.Expression, tl: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Vector Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the vector.
      * @param sp2  the position of the last character in the expression.
      */
    case class FVec(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Set Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the set.
      * @param sp2  the position of the last character in the expression.
      */
    case class FSet(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Map Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the (key, values) of the map.
      * @param sp2  the position of the last character in the expression.
      */
    case class FMap(sp1: SourcePosition, elms: Seq[(ParsedAst.Expression, ParsedAst.Expression)], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Get Index Expression (gets a value from a vector).
      *
      * @param sp1  the position of the first character in the expression.
      * @param exp1 the vector expression.
      * @param exp2 the index expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class GetIndex(sp1: SourcePosition, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Put Index Expression (puts a value into a vector).
      *
      * @param sp1  the position of the first character in the expression.
      * @param exp1 the vector expression.
      * @param exp2 the index expression.
      * @param exp3 the value expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class PutIndex(sp1: SourcePosition, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, exp3: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Existentially Quantified Expression.
      *
      * @param sp1       the position of the first character in the expression.
      * @param paramsOpt the existentially quantified variables.
      * @param exp       the existentially quantified expression.
      * @param sp2       the position of the last character in the expression.
      */
    case class Existential(sp1: SourcePosition, paramsOpt: Option[Seq[Ast.FormalParam]], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Universally Quantified Expression.
      *
      * @param sp1       the position of the first character in the expression.
      * @param paramsOpt the universally quantified variables.
      * @param exp       the universally quantified expression.
      * @param sp2       the position of the last character in the expression.
      */
    case class Universal(sp1: SourcePosition, paramsOpt: Option[Seq[Ast.FormalParam]], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Ascribe Expression.
      *
      * @param exp the expression.
      * @param tpe the ascribed type.
      * @param sp2 the position of the last character in the expression.
      */
    case class Ascribe(exp: ParsedAst.Expression, tpe: Type, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * User Error Expression (an expression that immediately aborts execution).
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class UserError(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Bot Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Bot(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Top Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Top(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

  }

  /**
    * Patterns.
    */
  sealed trait Pattern extends ParsedAst {

    /**
      * Returns the left most source position in sub-tree of `this` pattern.
      */
    def leftMostSourcePosition: SourcePosition = this match {
      case Pattern.Wild(sp1, _) => sp1
      case Pattern.Var(sp1, _, _) => sp1
      case Pattern.Lit(sp1, _, _) => sp1
      case Pattern.Tag(sp1, _, _, _, _) => sp1
      case Pattern.Tuple(sp1, _, _) => sp1
      case Pattern.FNone(sp1, _) => sp1
      case Pattern.FSome(sp1, _, _) => sp1
      case Pattern.FNil(sp1, sp2) => sp1
      case Pattern.FList(hd, _, _) => hd.leftMostSourcePosition
      case Pattern.FVec(sp1, _, _, _) => sp1
      case Pattern.FSet(sp1, _, _, _) => sp1
      case Pattern.FMap(sp1, _, _, _) => sp1
    }

  }

  object Pattern {

    /**
      * Literal Pattern.
      *
      * Inlined by the Weeder.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param lit the literal.
      * @param sp2 the position of the last character in the pattern.
      */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Wildcard Pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class Wild(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Variable Pattern.
      *
      * @param sp1   the position of the first character in the pattern.
      * @param ident the variable name.
      * @param sp2   the position of the last character in the pattern.
      */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Tag Pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param enum the enum name.
      * @param tag  the tag name.
      * @param pat  the optional value pattern.
      * @param sp2  the position of the last character in the pattern.
      */
    case class Tag(sp1: SourcePosition, enum: Name.QName, tag: Name.Ident, pat: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Tuple Pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the pattern.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * None Pattern (None is of type Opt[A]).
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FNone(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Some Pattern (Some(v) is of type Opt[A]).
      *
      * @param sp1 the position of the first character in the pattern.
      * @param pat the value pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FSome(sp1: SourcePosition, pat: ParsedAst.Pattern, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Nil Pattern (the empty list pattern).
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FNil(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * List Pattern (cons cell).
      *
      * @param hd  the head pattern.
      * @param tl  the tail pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FList(hd: ParsedAst.Pattern, tl: ParsedAst.Pattern, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Vector Pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param elms the elements of the vector.
      * @param rest the optional rest pattern.
      * @param sp2  the position of the last character in the pattern.
      */
    case class FVec(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], rest: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Set Pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param elms the elements of the set.
      * @param rest the optional rest pattern.
      * @param sp2  the position of the last character in the pattern.
      */
    case class FSet(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], rest: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Map Pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param elms the elements of the map.
      * @param rest the optional rest pattern.
      * @param sp2  the position of the last character in the pattern.
      */
    case class FMap(sp1: SourcePosition, elms: Seq[(ParsedAst.Pattern, ParsedAst.Pattern)], rest: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

  }

  /**
    * Predicates.
    */
  sealed trait Predicate extends ParsedAst

  object Predicate {

    /**
      * True Predicate.
      *
      * @param sp1 the position of the first character in the predicate.
      * @param sp2 the position of the last character in the predicate.
      */
    case class True(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Predicate

    /**
      * False Predicate.
      *
      * @param sp1 the position of the first character in the predicate.
      * @param sp2 the position of the last character in the predicate.
      */
    case class False(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Predicate

    /**
      * Ambiguous Predicate.
      *
      * @param sp1   the position of the first character in the predicate.
      * @param name  the unresolved name of the predicate.
      * @param terms the terms of the predicate.
      * @param sp2   the position of the last character in the predicate.
      */
    case class Ambiguous(sp1: SourcePosition, name: Name.QName, terms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Predicate

    /**
      * Equal Predicate.
      *
      * @param sp1   the position of the first character in the predicate.
      * @param ident the name of the variable.
      * @param term  the term.
      * @param sp2   the position of the last character in the predicate.
      */
    case class Equal(sp1: SourcePosition, ident: Name.Ident, term: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate

    /**
      * NotEqual Predicate.
      *
      * @param sp1    the position of the first character in the predicate.
      * @param ident1 the name of the first variable.
      * @param ident2 the name of the second variable.
      * @param sp2    the position of the last character in the predicate.
      */
    case class NotEqual(sp1: SourcePosition, ident1: Name.Ident, ident2: Name.Ident, sp2: SourcePosition) extends ParsedAst.Predicate

    /**
      * Loop Predicate.
      *
      * @param sp1   the position of the first character in the predicate.
      * @param ident the loop variable.
      * @param term  the set term.
      * @param sp2   the position of the last character in the predicate.
      */
    case class Loop(sp1: SourcePosition, ident: Name.Ident, term: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate

  }

  /**
    * Annotation.
    *
    * @param sp1  the position of the first character in the annotation.
    * @param name the name of the annotation.
    * @param sp2  the position of the last character in the annotation.
    */
  case class Annotation(sp1: SourcePosition, name: String, sp2: SourcePosition) extends ParsedAst

  /**
    * Case (member of an enum).
    *
    * @param sp1   the position of the first character in the case declaration.
    * @param ident the name of the declared tag.
    * @param tpe   the type of the declared tag
    * @param sp2   the position of the last character in the case declaration.
    */
  case class Case(sp1: SourcePosition, ident: Name.Ident, tpe: Type, sp2: SourcePosition) extends ParsedAst

  /**
    * Context Bound.
    *
    * @param sp1     the position of the first character in the context bound.
    * @param ident   the name of the type class.
    * @param tparams the type params of the class.
    * @param sp2     the position of the last character in the context bound.
    */
  case class ContextBound(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[Type], sp2: SourcePosition) extends ParsedAst

}
