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
  case class Program(roots: List[ParsedAst.Root], named: Map[Symbol.DefnSym, ParsedAst.Expression], time: Time) extends ParsedAst

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
      * Definition Declaration.
      *
      * @param doc        the optional comment associated with the definition.
      * @param ann        the associated annotations.
      * @param mod        the associated modifiers.
      * @param sp1        the position of the first character in the declaration.
      * @param ident      the name of the definition.
      * @param tparams    the type parameters.
      * @param fparamsOpt the formal parameters.
      * @param tpe        the declared type.
      * @param exp        the expression.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Def(doc: ParsedAst.Doc, ann: Seq[ParsedAst.AnnotationOrProperty], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Effect], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Effect Declaration.
      *
      * @param doc        the optional comment associated with the definition.
      * @param ann        the associated annotations.
      * @param mod        the associated modifiers.
      * @param sp1        the position of the first character in the declaration.
      * @param ident      the name of the definition.
      * @param tparams    the type parameters.
      * @param fparamsOpt the formal parameters.
      * @param tpe        the declared type.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Eff(doc: ParsedAst.Doc, ann: Seq[ParsedAst.AnnotationOrProperty], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Effect], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Handler Declaration.
      *
      * @param doc        the optional comment associated with the definition.
      * @param ann        the associated annotations.
      * @param mod        the associated modifiers.
      * @param sp1        the position of the first character in the declaration.
      * @param ident      the name of the definition.
      * @param tparams    the type parameters.
      * @param fparamsOpt the formal parameters.
      * @param tpe        the declared type.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Handler(doc: ParsedAst.Doc, ann: Seq[ParsedAst.AnnotationOrProperty], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Effect], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Signature Declaration.
      *
      * @param doc        the optional comment associated with the definition.
      * @param ann        the associated annotations.
      * @param mod        the associated modifiers.
      * @param sp1        the position of the first character in the declaration.
      * @param ident      the name of the definition.
      * @param tparams    the type parameters.
      * @param fparamsOpt the formal parameters.
      * @param tpe        the declared type.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Sig(doc: ParsedAst.Doc, ann: Seq[ParsedAst.AnnotationOrProperty], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Effect], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Law Declaration.
      *
      * @param sp1        the position of the first character in the declaration.
      * @param ident      the name of the law.
      * @param tparams    the type parameters.
      * @param fparamsOpt the value parameters.
      * @param tpe        the declared type.
      * @param exp        the expression.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Law(doc: ParsedAst.Doc, sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Enum Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the enum.
      * @param tparams the type parameters.
      * @param cases   the cases of the enum.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Enum(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], cases: Seq[ParsedAst.Case], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Type Declaration. A type declaration is syntactic sugar for a singleton enum declaration.
      *
      * @param doc   the optional comment associated with the declaration.
      * @param mod   the associated modifiers.
      * @param sp1   the position of the first character in the declaration.
      * @param ident the name of the type.
      * @param caze  the singleton case of the type.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Type(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, caze: ParsedAst.Case, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Relation Declaration.
      *
      * @param doc   the optional comment associated with the definition.
      * @param sp1   the position of the first character in the declaration.
      * @param ident the name of the relation.
      * @param attr  the attributes (columns) of the relation.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Relation(doc: ParsedAst.Doc, sp1: SourcePosition, ident: Name.Ident, attr: Seq[ParsedAst.Attribute], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Lattice Declaration.
      *
      * @param doc   the optional comment associated with the definition.
      * @param sp1   the position of the first character in the declaration.
      * @param ident the name of the lattice.
      * @param attr  the attributes (columns) of the relation.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Lattice(doc: ParsedAst.Doc, sp1: SourcePosition, ident: Name.Ident, attr: Seq[ParsedAst.Attribute], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Index Declaration.
      *
      * @param sp1     the position of the first character in the declaration.
      * @param qname   the name of the relation or lattice.
      * @param indexes the sequence of indexes.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Index(sp1: SourcePosition, qname: Name.QName, indexes: Seq[Seq[Name.Ident]], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Constraint Declaration.
      *
      * @param sp1  the position of the first character in the declaration.
      * @param head the head predicates (a conjunction of predicates).
      * @param body the body predicates (a sequence of disjunctions of predicates).
      * @param sp2  the position of the last character in the declaration.
      */
    case class Constraint(sp1: SourcePosition, head: Seq[ParsedAst.Predicate.Head], body: Seq[Seq[ParsedAst.Predicate.Body]], sp2: SourcePosition) extends ParsedAst.Declaration

    case class BoundedLattice(sp1: SourcePosition, tpe: ParsedAst.Type, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Class Declaration.
      *
      * @param doc   the optional comment associated with the declaration.
      * @param sp1   the position of the first character in the declaration.
      * @param mod   the associated modifiers.
      * @param cc    the class constraint.
      * @param decls the declarations of the class.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Class(doc: ParsedAst.Doc, sp1: SourcePosition, mod: Seq[ParsedAst.Modifier], cc: ParsedAst.ClassConstraint, decls: Seq[ParsedAst.Declaration], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Impl Declaration.
      *
      * @param doc  the optional comment associated with the declaration.
      * @param sp1  the position of the first character in the declaration.
      * @param mod  the associated modifiers.
      * @param ic   the impl constraint.
      * @param defs the definitions of the instance.
      * @param sp2  the position of the last character in the declaration.
      */
    case class Impl(doc: ParsedAst.Doc, sp1: SourcePosition, mod: Seq[ParsedAst.Modifier], ic: ParsedAst.ImplConstraint, defs: Seq[ParsedAst.Declaration.Def], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Disallow Declaration.
      *
      * @param doc the optional comment associated with the declaration.
      * @param sp1 the position of the first character in the declaration.
      * @param ic  the integrity constraint.
      * @param sp2 the position of the last character in the declaration.
      */
    case class Disallow(doc: ParsedAst.Doc, sp1: SourcePosition, ic: ParsedAst.DisallowConstraint, sp2: SourcePosition) extends ParsedAst.Declaration

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
      * Simple Name Expression (either a variable or reference expression).
      *
      * @param sp1  the position of the first character in the expression.
      * @param name the name.
      * @param sp2  the position of the last character in the expression.
      */
    case class SName(sp1: SourcePosition, name: Name.Ident, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Qualified Name Expression (reference expression).
      *
      * @param sp1  the position of the first character in the expression.
      * @param name the name.
      * @param sp2  the position of the last character in the expression.
      */
    case class QName(sp1: SourcePosition, name: Name.QName, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Hole Expression.
      *
      * @param sp1   the position of the first character in the expression
      * @param ident the name of the hole.
      * @param sp2   the position of the last character in the expression.
      */
    case class Hole(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Literal Expression.
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
      * Infix Apply.
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
      * Postfix Apply.
      *
      * Replaced with Apply by Weeder.
      *
      * @param e    the first argument expression.
      * @param name the name of the function.
      * @param es   the the remaining arguments.
      * @param sp2  the position of the last character in the expression.
      */
    case class Postfix(e: ParsedAst.Expression, name: Name.Ident, es: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Lambda Expression.
      *
      * @param sp1     the position of the first character in the expression.
      * @param fparams the formal parameters.
      * @param exp     the body expression.
      * @param sp2     the position of the last character in the expression.
      */
    case class Lambda(sp1: SourcePosition, fparams: Seq[ParsedAst.FormalParam], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Lambda Match Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param pat the pattern.
      * @param exp the body.
      * @param sp2 the position of the last character in the expression.
      */
    case class LambdaMatch(sp1: SourcePosition, pat: ParsedAst.Pattern, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Unary Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param op  the unary operator.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Unary(sp1: SourcePosition, op: String, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Binary Expression.
      *
      * @param exp1 the left expression.
      * @param op   the binary operator.
      * @param exp2 the right expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Binary(exp1: ParsedAst.Expression, op: String, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param tpe  the optional type annotation.
      * @param exp1 the value expression.
      * @param exp2 the body expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class LetMatch(sp1: SourcePosition, pat: ParsedAst.Pattern, tpe: Option[ParsedAst.Type], exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * LetRec Expression (recursive let-binding).
      *
      * @param sp1   the position of the first character in the expression.
      * @param ident the bound identifier.
      * @param exp1  the value expression.
      * @param exp2  the body expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class LetRec(sp1: SourcePosition, ident: Name.Ident, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Match Expression (pattern match expression).
      *
      * @param sp1   the position of the first character in the expression.
      * @param exp   the value expression.
      * @param rules the rules of the pattern match.
      * @param sp2   the position of the last character in the expression.
      */
    case class Match(sp1: SourcePosition, exp: ParsedAst.Expression, rules: Seq[ParsedAst.MatchRule], sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param name the optionally fully-qualified name of the type and the tag name.
      * @param exp  the optional value expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Tag(sp1: SourcePosition, name: Name.QName, exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Tuple Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the expression.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArrayLit Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param elms the elements of the array.
      * @param sp2 the position of the last character in the expression.
      */
    case class ArrayLit(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArrayNew Expression
      *
      * @param sp1 the position of the first character in the expression.
      * @param elm the default value of the array elements.
      * @param len the length of the array.
      * @param sp2 the position of the last character in the expression.
      */
    case class ArrayNew(sp1: SourcePosition, elm: ParsedAst.Expression, len: ParsedAst.Expression, sp2: SourcePosition) extends  ParsedAst.Expression

    /**
      * ArrayLoad Expression
      *
      * @param base the array.
      * @param index the index to load from.
      * @param sp2 the position of the last character in the expression.
      */
    case class ArrayLoad(base: ParsedAst.Expression, index: ParsedAst.Expression, sp2: SourcePosition) extends  ParsedAst.Expression

    /**
      * ArrayStore Expression
      *
      * @param base the array.
      * @param indexes the indexes to load from and the last to store into.
      * @param elm the element to store into the given index.
      * @param sp2 the position of the last character in the expression.
      */
    case class ArrayStore(base: ParsedAst.Expression, indexes: Seq[ParsedAst.Expression], elm: ParsedAst.Expression, sp2: SourcePosition) extends  ParsedAst.Expression

    /**
      * ArrayLenght Expression
      *
      * @param sp1 the position of the first character in the expression.
      * @param base the array
      * @param sp2 the position of the last character in the expression.
      */
    case class ArrayLength(sp1: SourcePosition, base: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArraySlice Expression
      *
      * @param base the array
      * @param beginIndex the start index
      * @param endIndex the end index
      * @param sp2 the position of the last character in the expression.
      */
    case class ArraySlice(base: ParsedAst.Expression, beginIndex: Option[ParsedAst.Expression], endIndex: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * VecLit Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param elms the elements of the vector.
      * @param sp2 the position of the last character in the expression.
      */
    case class VectorLit(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * VectorNew Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param elm the default value the elements.
      * @param len the length of the vector.
      * @param sp2 the position of the last character in the expression.
      */
    case class VectorNew(sp1: SourcePosition, elm: ParsedAst.Expression, len: ParsedAst.Literal, sp2: SourcePosition) extends  ParsedAst.Expression

    /**
      * VectorLoad Expression.
      *
      * @param base the vector variable which is loaded from.
      * @param index the index to load.
      * @param sp2 the position of the last character in the expression.
      */
    case class VectorLoad(base: ParsedAst.Expression, index: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * VectorStore Expression.
      *
      * @param base the vector variable which is stored in.
      * @param indexes the indexes to load from and the last to store the element in.
      * @param elm the expression to be stored.
      * @param sp2 the position of the last character in the expression.
      */
    case class VectorStore(base: ParsedAst.Expression, indexes: Seq[ParsedAst.Literal], elm: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * VectorLength Expression
      *
      * @param sp1 the position of the first character in the expression.
      * @param base the vector to find the length of.
      * @param sp2 the position of the last character in the expression.
      */
    case class VectorLength(sp1: SourcePosition, base: Expression, sp2: SourcePosition) extends  ParsedAst.Expression

    /**
      * VectorSlice Expression.
      *
      * @param base the vector for slice.
      * @param optStartIndex the start index.
      * @param optEndIndex the end index.
      * @param sp2 the position of the last character in the expression.
      */
    case class VectorSlice(base: Expression, optStartIndex: Option[Literal], optEndIndex: Option[Literal], sp2: SourcePosition) extends  ParsedAst.Expression

    /**
      * Unique Expression.
      *
      * @param sp1 the position of the first character in the unique keyword.
      * @param exp the exp to be unique.
      * @param sp2 the position of the last character in the expression.
      */
    case class Unique(sp1: SourcePosition, exp: Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Nil Expression (of list).
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class FNil(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Cons expression (of list).
      *
      * @param hd  the head of the list.
      * @param sp1 the position of the first character in the :: operator.
      * @param sp2 the position of the last character in the :: operator.
      * @param tl  the tail of the list.
      */
    case class FCons(hd: ParsedAst.Expression, sp1: SourcePosition, sp2: SourcePosition, tl: ParsedAst.Expression) extends ParsedAst.Expression

    /**
      * Append expression (of list).
      *
      * @param fst the first list.
      * @param sp1 the position of the first character in the operator @@.
      * @param sp2 the position of the last character in the operator @@.
      * @param snd the second list.
      */
    case class FAppend(fst: ParsedAst.Expression, sp1: SourcePosition, sp2: SourcePosition, snd: ParsedAst.Expression) extends ParsedAst.Expression

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
      * Reference expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression to reference.
      * @param sp2 the position of the last character in the expression.
      */
    case class Ref(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Dereference expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression to dereference.
      * @param sp2 the position of the last character in the expression.
      */
    case class Deref(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Assignment expression.
      *
      * @param exp1 the expression to reference.
      * @param exp2 the expression to assign.
      * @param sp2  the position of the last character in the expression.
      */
    case class Assign(exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * HandleWith expression.
      *
      * @param sp1      the position of the first character in the expression.
      * @param exp      the base expression.
      * @param bindings the effect handler bindings.
      * @param sp2      the position of the last character in the expression.
      */
    case class HandleWith(sp1: SourcePosition, exp: ParsedAst.Expression, bindings: Seq[ParsedAst.HandlerBinding], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Existentially Quantified Expression.
      *
      * @param sp1        the position of the first character in the expression.
      * @param fparamsOpt the existentially quantified variables.
      * @param exp        the existentially quantified expression.
      * @param sp2        the position of the last character in the expression.
      */
    case class Existential(sp1: SourcePosition, fparamsOpt: Seq[ParsedAst.FormalParam], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Universally Quantified Expression.
      *
      * @param sp1        the position of the first character in the expression.
      * @param fparamsOpt the universally quantified variables.
      * @param exp        the universally quantified expression.
      * @param sp2        the position of the last character in the expression.
      */
    case class Universal(sp1: SourcePosition, fparamsOpt: Seq[ParsedAst.FormalParam], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Ascribe Expression.
      *
      * @param exp the expression.
      * @param tpe the type.
      * @param eff the effect.
      * @param sp2 the position of the last character in the expression.
      */
    case class Ascribe(exp: ParsedAst.Expression, tpe: ParsedAst.Type, eff: Option[ParsedAst.Effect], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Cast Expression.
      *
      * @param exp the expression.
      * @param tpe the type.
      * @param eff the effect.
      * @param sp2 the position of the last character in the expression.
      */
    case class Cast(exp: ParsedAst.Expression, tpe: ParsedAst.Type, eff: Option[ParsedAst.Effect], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Unsafe Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the unsafe expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Unsafe(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Native Constructor Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param fqn  the fully-qualified name of the native class.
      * @param args the arguments to the constructor.
      * @param sp2  the position of the last character in the expression.
      */
    case class NativeConstructor(sp1: SourcePosition, fqn: Seq[String], args: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Native Field Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param fqn the fully-qualified name of the native field.
      * @param sp2 the position of the last character in the expression.
      */
    case class NativeField(sp1: SourcePosition, fqn: Seq[String], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Native Method Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param fqn  the fully-qualified name of the native method.
      * @param args the arguments to the method.
      * @param sp2  the position of the last character in the expression.
      */
    case class NativeMethod(sp1: SourcePosition, fqn: Seq[String], args: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * User Error Expression (an expression that immediately aborts execution).
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class UserError(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

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
      case Pattern.Tag(sp1, _, _, _) => sp1
      case Pattern.Tuple(sp1, _, _) => sp1
      case Pattern.FNil(sp1, _) => sp1
      case Pattern.FCons(hd, _, _, _) => hd.leftMostSourcePosition
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
      * @param name the optionally fully-qualified name of the type and the tag name.
      * @param pat  the optional value pattern.
      * @param sp2  the position of the last character in the pattern.
      */
    case class Tag(sp1: SourcePosition, name: Name.QName, pat: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Tuple Pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the pattern.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Nil Pattern (of list).
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FNil(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Cons Pattern (of list).
      *
      * @param hd  the head pattern.
      * @param sp1 the position of the first character in the :: operator.
      * @param sp2 the position of the last character in the :: operator.
      * @param tl  the tail pattern.
      */
    case class FCons(hd: ParsedAst.Pattern, sp1: SourcePosition, sp2: SourcePosition, tl: ParsedAst.Pattern) extends ParsedAst.Pattern

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

    sealed trait Head extends ParsedAst.Predicate

    object Head {

      /**
        * True Predicate.
        *
        * @param sp1 the position of the first character in the predicate.
        * @param sp2 the position of the last character in the predicate.
        */
      case class True(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Predicate.Head

      /**
        * False Predicate.
        *
        * @param sp1 the position of the first character in the predicate.
        * @param sp2 the position of the last character in the predicate.
        */
      case class False(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Predicate.Head

      /**
        * Atom Predicate.
        *
        * @param sp1   the position of the first character in the predicate.
        * @param name  the qualified name of the table.
        * @param terms the terms of the predicate.
        * @param sp2   the position of the last character in the predicate.
        */
      case class Atom(sp1: SourcePosition, name: Name.QName, terms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Predicate.Head

    }

    sealed trait Body extends ParsedAst.Predicate

    object Body {

      /**
        * Positive Predicate.
        *
        * @param sp1   the position of the first character in the predicate.
        * @param name  the qualified name of the table.
        * @param terms the terms of the predicate.
        * @param sp2   the position of the last character in the predicate.
        */
      case class Positive(sp1: SourcePosition, name: Name.QName, terms: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * Negative Predicate.
        *
        * @param sp1   the position of the first character in the predicate.
        * @param name  the qualified name of the table.
        * @param terms the terms of the predicate.
        * @param sp2   the position of the last character in the predicate.
        */
      case class Negative(sp1: SourcePosition, name: Name.QName, terms: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * Filter Predicate.
        *
        * @param sp1   the position of the first character in the predicate.
        * @param name  the qualified name of the filter function.
        * @param terms the terms of the predicate.
        * @param sp2   the position of the last character in the predicate.
        */
      case class Filter(sp1: SourcePosition, name: Name.QName, terms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * NotEqual Predicate.
        *
        * @param sp1    the position of the first character in the predicate.
        * @param ident1 the name of the first variable.
        * @param ident2 the name of the second variable.
        * @param sp2    the position of the last character in the predicate.
        */
      case class NotEqual(sp1: SourcePosition, ident1: Name.Ident, ident2: Name.Ident, sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * Loop Predicate.
        *
        * @param sp1  the position of the first character in the predicate.
        * @param pat  the loop pattern.
        * @param term the set term.
        * @param sp2  the position of the last character in the predicate.
        */
      case class Loop(sp1: SourcePosition, pat: ParsedAst.Pattern, term: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate.Body

    }

  }

  /**
    * Types.
    */
  sealed trait Type extends ParsedAst

  object Type {

    /**
      * Unit type.
      *
      * @param sp1 the position of the first character in the type.
      * @param sp2 the position of the last character in the type.
      */
    case class Unit(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Type Variable.
      *
      * @param sp1   the position of the first character in the type.
      * @param ident the variable name.
      * @param sp2   the position of the last character in the type.
      */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Primitive or Named type.
      *
      * @param sp1   the position of the first character in the type.
      * @param qname the qualified name of the type.
      * @param sp2   the position of the last character in the type.
      */
    case class Ambiguous(sp1: SourcePosition, qname: Name.QName, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Tuple Type.
      *
      * @param sp1  the position of the first character in the type.
      * @param elms the types of the elements.
      * @param sp2  the position of the last character in the type.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Type
    
    /**
      * Nat Type.
      *
      * @param sp1 the position of the first character in the type.
      * @param len the type of the element.
      * @param sp2 the position of the last character in the type.
      */
    case class Nat(sp1: SourcePosition, len: ParsedAst.Literal.Int32, sp2: SourcePosition) extends ParsedAst.Type


    /**
      * Arrow Type.
      *
      * @param sp1     the position of the first character in the type.
      * @param tparams the arguments types.
      * @param tresult the result type.
      * @param sp2     the position of the last character in the type.
      */
    case class Arrow(sp1: SourcePosition, tparams: Seq[ParsedAst.Type], tresult: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Infix Type Application.
      *
      * @param tpe1 the first type parameter.
      * @param base the base type.
      * @param tpe2 the second type parameter.
      * @param sp2  the position of the last character in the type.
      */
    case class Infix(tpe1: ParsedAst.Type, base: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Native Type.
      *
      * @param sp1 the position of the first character in the type.
      * @param fqn the fully qualified Java name.
      * @param sp2 the position of the last character in the type.
      */
    case class Native(sp1: SourcePosition, fqn: Seq[String], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Type Application.
      *
      * @param base    the base type.
      * @param tparams the type parameters.
      * @param sp2     the position of the last character in the type.
      */
    case class Apply(base: ParsedAst.Type, tparams: Seq[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Type

  }

  /**
    * Effects.
    */
  case class Effect(xs: Seq[Name.Ident]) extends ParsedAst

  /**
    * Attribute.
    *
    * @param sp1   the position of the first character in the attribute.
    * @param ident the name of the attribute.
    * @param tpe   the type of the attribute.
    * @param sp2   the position of the last character in the annotation.
    */
  case class Attribute(sp1: SourcePosition, ident: Name.Ident, tpe: ParsedAst.Type, sp2: SourcePosition)

  /**
    * Case (member of an enum).
    *
    * @param sp1   the position of the first character in the case declaration.
    * @param ident the name of the declared tag.
    * @param tpe   the type of the declared tag
    * @param sp2   the position of the last character in the case declaration.
    */
  case class Case(sp1: SourcePosition, ident: Name.Ident, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst

  /**
    * Class Constraint.
    *
    * @param head the head atom of the constraint.
    * @param body the sequence of body atoms of the constraint.
    */
  case class ClassConstraint(head: ParsedAst.SimpleClass, body: Seq[ParsedAst.SimpleClass]) extends ParsedAst

  /**
    * Impl Constraint.
    *
    * @param head the head atom of the constraint.
    * @param body the sequence of body atoms of the constraint.
    */
  case class ImplConstraint(head: ParsedAst.ComplexClass, body: Seq[ParsedAst.ComplexClass]) extends ParsedAst

  /**
    * Disallow Constraint.
    */
  case class DisallowConstraint(body: Seq[ParsedAst.ComplexClass]) extends ParsedAst

  /**
    * Simple Class Atom.
    *
    * @param sp1   the position of the first character in the atom.
    * @param qname the (qualified) class name.
    * @param args  the type variables.
    * @param sp2   the position of the last character in the atom.
    */
  case class SimpleClass(sp1: SourcePosition, qname: Name.QName, args: Seq[Name.Ident], sp2: SourcePosition) extends ParsedAst

  sealed trait ComplexClass

  object ComplexClass {

    /**
      * Positive Complex Class Atom.
      *
      * @param sp1   the position of the first character in the atom.
      * @param qname the (qualified) class name.
      * @param args  the type arguments.
      * @param sp2   the position of the last character in the atom.
      */
    case class Positive(sp1: SourcePosition, qname: Name.QName, args: Seq[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.ComplexClass

    /**
      * Negative Complex Class Atom.
      *
      * @param sp1   the position of the first character in the atom.
      * @param qname the (qualified) class name.
      * @param args  the type arguments.
      * @param sp2   the position of the last character in the atom.
      */
    case class Negative(sp1: SourcePosition, qname: Name.QName, args: Seq[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.ComplexClass

  }


  /**
    * Documentation.
    *
    * @param sp1   the position of the first character in the comment.
    * @param lines the lines of the comment.
    * @param sp2   the position of the last character in the comment.
    */
  case class Doc(sp1: SourcePosition, lines: Seq[String], sp2: SourcePosition) extends ParsedAst

  /**
    * Context Bound.
    *
    * @param sp1     the position of the first character in the context bound.
    * @param ident   the name of the type class.
    * @param tparams the type params of the class.
    * @param sp2     the position of the last character in the context bound.
    */
  case class ContextBound(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst

  /**
    * Formal Parameter.
    *
    * @param sp1   the position of the first character in the formal parameter.
    * @param mod   the associated modifiers.
    * @param ident the name of the argument.
    * @param tpe   the optional type of the argument.
    * @param sp2   the position of the last character in the formal parameter.
    */
  case class FormalParam(sp1: SourcePosition, mod: Seq[ParsedAst.Modifier], ident: Name.Ident, tpe: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst

  /**
    * A binding of an effect to an effect handler expression.
    *
    * @param qname the fully-qualified name of the effect.
    * @param exp   the expression with which to handle the effect.
    */
  case class HandlerBinding(qname: Name.QName, exp: ParsedAst.Expression) extends ParsedAst

  /**
    * A pattern match rule consists of a pattern, an optional pattern guard, and a body expression.
    *
    * @param pat   the pattern of the rule.
    * @param guard the optional guard of the rule.
    * @param exp   the body expression of the rule.
    */
  case class MatchRule(pat: ParsedAst.Pattern, guard: Option[ParsedAst.Expression], exp: ParsedAst.Expression) extends ParsedAst

  /**
    * Modifier.
    *
    * @param sp1  the position of the first character in the modifier.
    * @param name the name of the modifier.
    * @param sp2  the position of the last character in the modifier.
    */
  case class Modifier(sp1: SourcePosition, name: String, sp2: SourcePosition) extends ParsedAst

  /**
    * A common super-type for annotations or properties.
    */
  sealed trait AnnotationOrProperty extends ParsedAst

  /**
    * Annotation.
    *
    * @param sp1   the position of the first character in the annotation.
    * @param ident the name of the annotation.
    * @param sp2   the position of the last character in the annotation.
    */
  case class Annotation(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends AnnotationOrProperty

  /**
    * Property.
    *
    * @param sp1  the position of the first character in the property.
    * @param law  the qualified name of the law.
    * @param args the optional arguments of the property.
    * @param sp2  the position of the last character in the property.
    */
  case class Property(sp1: SourcePosition, law: Name.QName, args: Option[Seq[ParsedAst.Expression]], sp2: SourcePosition) extends AnnotationOrProperty

}
