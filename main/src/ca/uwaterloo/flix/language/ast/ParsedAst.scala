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

import ca.uwaterloo.flix.language.ast.Ast.Polarity

import scala.collection.immutable.Seq

object ParsedAst {

  /**
    * Program. A collection of abstract syntax trees.
    *
    * @param roots the roots of the abstract syntax trees in the program.
    */
  case class Program(roots: List[ParsedAst.Root])

  /**
    * Root. A collection of imports and declarations.
    *
    * @param sp1   the position of the first character in the source.
    * @param uses  the uses in the abstract syntax tree.
    * @param decls the declarations in the abstract syntax tree.
    * @param sp2   the position of the last character in the source.
    */
  case class Root(sp1: SourcePosition, uses: Seq[ParsedAst.Use], decls: Seq[ParsedAst.Declaration], sp2: SourcePosition)

  /**
    * Declarations.
    */
  sealed trait Declaration

  object Declaration {

    /**
      * Namespace Declaration.
      *
      * @param sp1   the position of the first character in the declaration.
      * @param name  the name of the namespace.
      * @param uses  the uses available in the namespace.
      * @param decls the nested declarations.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Namespace(sp1: SourcePosition, name: Name.NName, uses: Seq[ParsedAst.Use], decls: Seq[ParsedAst.Declaration], sp2: SourcePosition) extends ParsedAst.Declaration

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
    case class Def(doc: ParsedAst.Doc, ann: Seq[ParsedAst.AnnotationOrProperty], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Type], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

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
    case class Sig(doc: ParsedAst.Doc, ann: Seq[ParsedAst.AnnotationOrProperty], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Declaration

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
    case class Law(doc: ParsedAst.Doc, sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

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
    case class Enum(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, cases: Seq[ParsedAst.Case], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Opaque Type Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the opaque type.
      * @param tparams the type parameters of the opaque type.
      * @param tpe     the type of the opaque type.
      * @param sp2     the position of the last character in the declaration.
      */
    case class OpaqueType(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Type Alias Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the opaque type.
      * @param tparams the type parameters of the opaque type.
      * @param tpe     the type of the opaque type.
      * @param sp2     the position of the last character in the declaration.
      */
    case class TypeAlias(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Relation Declaration.
      *
      * @param doc     the optional comment associated with the definition.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the relation.
      * @param tparams the type parameters.
      * @param attr    the attributes (columns) of the relation.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Relation(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, attr: Seq[ParsedAst.Attribute], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Lattice Declaration.
      *
      * @param doc     the optional comment associated with the definition.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the lattice.
      * @param tparams the type parameters.
      * @param attr    the attributes (columns) of the relation.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Lattice(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, attr: Seq[ParsedAst.Attribute], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Constraint Declaration.
      *
      * @param sp1  the position of the first character in the declaration.
      * @param head the head predicate.
      * @param body the body predicates.
      * @param sp2  the position of the last character in the declaration.
      */
    case class Constraint(sp1: SourcePosition, head: ParsedAst.Predicate.Head, body: Seq[ParsedAst.Predicate.Body], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Typeclass Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the definition.
      * @param tparams the type parameters.
      * @param sigs    the signatures of the class.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Class(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, sigs: Seq[ParsedAst.Declaration.Sig], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Typeclass instance.
      *
      * @param doc   the optional comment associated with the declaration.
      * @param mod   the associated modifiers.
      * @param sp1   the position of the first character in the declaration.
      * @param clazz the name of the class.
      * @param tpe   the type of the instance.
      * @param defs  the definitions of the instance.
      * @param sp2   the position of the last character in the declaration.
      */
    case class Instance(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, clazz: Name.QName, tpe: ParsedAst.Type, defs: Seq[ParsedAst.Declaration.Def], sp2: SourcePosition) extends ParsedAst.Declaration

    case class LatticeComponents(sp1: SourcePosition, tpe: ParsedAst.Type, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Declaration

  }

  /**
    * Uses.
    */
  sealed trait Use

  object Use {

    /**
      * A use of a single class from a namespace.
      *
      * @param sp1   the position of the first character.
      * @param nname the namespace.
      * @param ident the name.
      * @param sp2   the position of the last character.
      */
    case class UseClass(sp1: SourcePosition, nname: Name.NName, ident: Name.Ident, sp2: SourcePosition) extends Use

    /**
      * A use of a single name from a namespace.
      *
      * @param sp1   the position of the first character.
      * @param nname the namespace.
      * @param ident the name.
      * @param sp2   the position of the last character.
      */
    case class UseOne(sp1: SourcePosition, nname: Name.NName, ident: Name.Ident, sp2: SourcePosition) extends Use

    /**
      * A use of multiple names from a namespace.
      *
      * @param sp1   the position of the first character.
      * @param nname the namespace.
      * @param names the names.
      * @param sp2   the position of the last character.
      */
    case class UseMany(sp1: SourcePosition, nname: Name.NName, names: Seq[ParsedAst.Use.NameAndAlias], sp2: SourcePosition) extends Use

    /**
      * A use of a single tag.
      *
      * @param sp1   the position of the first character.
      * @param qname the name of the enum.
      * @param tag   the name of the tag.
      * @param sp2   the position of the last character.
      */
    case class UseOneTag(sp1: SourcePosition, qname: Name.QName, tag: Name.Ident, sp2: SourcePosition) extends Use

    /**
      * A use of multiple tags.
      *
      * @param sp1   the position of the first character.
      * @param qname the name of the enum.
      * @param tags  the names of the tags.
      * @param sp2   the position of the last character.
      */
    case class UseManyTag(sp1: SourcePosition, qname: Name.QName, tags: Seq[ParsedAst.Use.NameAndAlias], sp2: SourcePosition) extends Use

    /**
      * A name with an optional alias.
      *
      * @param sp1   the position of the first character.
      * @param ident the name.
      * @param alias the optional alias.
      * @param sp2   the position of the last character.
      */
    case class NameAndAlias(sp1: SourcePosition, ident: Name.Ident, alias: Option[Name.Ident], sp2: SourcePosition)

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
      * Null Literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Null(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Literal

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
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign (true if signed).
      * @param radix the radix of the literal.
      * @param lit   the int8 literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class Int8(sp1: SourcePosition, sign: Boolean, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int16 Literal (signed 16-bit integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign (true if signed).
      * @param radix the radix of the literal.
      * @param lit   the int16 literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class Int16(sp1: SourcePosition, sign: Boolean, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int32 Literal (signed 32-bit integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign (true if signed).
      * @param radix the radix of the literal.
      * @param lit   the int32 literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class Int32(sp1: SourcePosition, sign: Boolean, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int64 Literal (signed 64-bit integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign (true if signed).
      * @param radix the radix of the literal.
      * @param lit   the int64 literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class Int64(sp1: SourcePosition, sign: Boolean, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * BigInt Literal (arbitrary sized integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign (true if signed).
      * @param radix the radix of the literal.
      * @param lit   the big int literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class BigInt(sp1: SourcePosition, sign: Boolean, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * String Literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the string literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Str(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Default Literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Default(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Literal

  }

  /**
    * Expressions.
    */
  sealed trait Expression

  object Expression {

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
      * @param ident the optional name of the hole.
      * @param sp2   the position of the last character in the expression.
      */
    case class Hole(sp1: SourcePosition, ident: Option[Name.Ident], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Use Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param use the use.
      * @param exp the body expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Use(sp1: SourcePosition, use: ParsedAst.Use, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * Statement Expression.
      *
      * @param exp1 the first expression.
      * @param exp2 the second expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Stm(exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * LetMatchStar Expression (monadic let-binding with pattern match).
      *
      * @param sp1  the position of the first character in the expression.
      * @param pat  the match pattern.
      * @param tpe  the optional type annotation.
      * @param exp1 the value expression.
      * @param exp2 the body expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class LetMatchStar(sp1: SourcePosition, pat: ParsedAst.Pattern, tpe: Option[ParsedAst.Type], exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Let Import Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param op  the imported JVM operation.
      * @param exp the body expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class LetImport(sp1: SourcePosition, op: ParsedAst.JvmOp, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * Choose Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param exps  the match expressions.
      * @param rules the rules of the pattern match.
      * @param sp2   the position of the last character in the expression.
      */
    case class Choose(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], rules: Seq[ChoiceRule], sp2: SourcePosition) extends ParsedAst.Expression

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
      * Record Literal Expression.
      *
      * @param sp1    the position of the first character in the expression.
      * @param fields the field literals of the record.
      * @param sp2    the position of the last character in the expression.
      */
    case class RecordLit(sp1: SourcePosition, fields: Seq[ParsedAst.RecordField], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Record Select Expression.
      *
      * @param exp   the record expression.
      * @param label the label to select from the record.
      * @param sp2   the position of the last character in the expression.
      */
    case class RecordSelect(exp: ParsedAst.Expression, label: Name.Ident, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Record Select Lambda Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param label the label to select from the record.
      * @param sp2   the position of the last character in the expression.
      */
    case class RecordSelectLambda(sp1: SourcePosition, label: Name.Ident, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Record Operation Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param ops  the sequence of record operations.
      * @param rest the base record to apply the operation to.
      * @param sp2  the position of the last character in the expression.
      */
    case class RecordOperation(sp1: SourcePosition, ops: Seq[ParsedAst.RecordOp], rest: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArrayLit Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the array.
      * @param sp2  the position of the last character in the expression.
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
    case class ArrayNew(sp1: SourcePosition, elm: ParsedAst.Expression, len: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArrayLoad Expression
      *
      * @param base  the array.
      * @param index the index to load from.
      * @param sp2   the position of the last character in the expression.
      */
    case class ArrayLoad(base: ParsedAst.Expression, index: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArrayStore Expression
      *
      * @param base    the array.
      * @param indexes the indexes to load from and the last to store into.
      * @param elm     the element to store into the given index.
      * @param sp2     the position of the last character in the expression.
      */
    case class ArrayStore(base: ParsedAst.Expression, indexes: Seq[ParsedAst.Expression], elm: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArraySlice Expression
      *
      * @param base       the array
      * @param beginIndex the start index
      * @param endIndex   the end index
      * @param sp2        the position of the last character in the expression.
      */
    case class ArraySlice(base: ParsedAst.Expression, beginIndex: Option[ParsedAst.Expression], endIndex: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

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
      * String Interpolation Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param parts the parts of the interpolation.
      * @param sp2   the position of the last character in the expression.
      */
    case class Interpolation(sp1: SourcePosition, parts: Seq[InterpolationPart], sp2: SourcePosition) extends ParsedAst.Expression

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
      * Existentially Quantified Expression.
      *
      * @param sp1     the position of the first character in the expression.
      * @param tparams the type parameters.
      * @param fparams the existentially quantified variables.
      * @param exp     the existentially quantified expression.
      * @param sp2     the position of the last character in the expression.
      */
    case class Existential(sp1: SourcePosition, tparams: ParsedAst.TypeParams, fparams: Seq[ParsedAst.FormalParam], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Universally Quantified Expression.
      *
      * @param sp1     the position of the first character in the expression.
      * @param tparams the type parameters.
      * @param fparams the universally quantified variables.
      * @param exp     the universally quantified expression.
      * @param sp2     the position of the last character in the expression.
      */
    case class Universal(sp1: SourcePosition, tparams: ParsedAst.TypeParams, fparams: Seq[ParsedAst.FormalParam], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Ascribe Expression.
      *
      * @param exp the expression.
      * @param tpe the optional type.
      * @param eff the optional effect.
      * @param sp2 the position of the last character in the expression.
      */
    case class Ascribe(exp: ParsedAst.Expression, tpe: Option[ParsedAst.Type], eff: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Cast Expression.
      *
      * @param exp the expression.
      * @param tpe the optional type.
      * @param eff the optional effect.
      * @param sp2 the position of the last character in the expression.
      */
    case class Cast(exp: ParsedAst.Expression, tpe: Option[ParsedAst.Type], eff: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Try Catch Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param exp   the guarded expression.
      * @param rules the catch rules.
      * @param sp2   the position of the last character in the expression.
      */
    case class TryCatch(sp1: SourcePosition, exp: ParsedAst.Expression, rules: Seq[ParsedAst.CatchRule], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * NewChannel Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param tpe the type of the channel elements.
      * @param exp the size of the channel.
      * @param sp2 the position of the last character in the expression.
      */
    case class NewChannel(sp1: SourcePosition, tpe: ParsedAst.Type, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * GetChannel Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the channel expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class GetChannel(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * PutChannel Expression
      *
      * @param exp1 the channel expression.
      * @param exp2 the expression to put in the channel.
      * @param sp2  the position of the last character in the expression.
      */
    case class PutChannel(exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * SelectChannel Expression.
      *
      * @param sp1     the position of the first character in the expression.
      * @param rules   the rules of the select expression.
      * @param default the default of the select expression.
      * @param sp2     the position of the last character in the expression.
      */
    case class SelectChannel(sp1: SourcePosition, rules: Seq[SelectChannelRule], default: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Spawn Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Spawn(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Lazy Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Lazy(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Force Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Force(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Constraint expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param con the constraint.
      * @param sp2 the position of the last character in the expression.
      */
    case class FixpointConstraint(sp1: SourcePosition, con: Declaration.Constraint, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Constraint Set expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param cs  the set of constraints.
      * @param sp2 the position of the last character in the expression.
      */
    case class FixpointConstraintSet(sp1: SourcePosition, cs: Seq[Declaration.Constraint], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Compose expression.
      *
      * @param exp1 the first constraint expression.
      * @param exp2 the second constraint expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class FixpointCompose(exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Solve expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the constraint expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class FixpointSolve(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Project expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param ident the name of the predicate.
      * @param exp   the constraint expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class FixpointProject(sp1: SourcePosition, ident: Name.Ident, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Entails expression.
      *
      * @param exp1 the lhs expression.
      * @param exp2 the rhs expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class FixpointEntails(exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Fold expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param ident the name of the predicate.
      * @param exp1  the initial value.
      * @param exp2  the function to fold.
      * @param exp3  the constraints over which to fold.
      * @param sp2   the position of the last character in the expression.
      */
    case class FixpointFold(sp1: SourcePosition, ident: Name.Ident, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, exp3: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

  }

  /**
    * Patterns.
    */
  sealed trait Pattern {

    /**
      * Returns the left most source position in sub-tree of `this` pattern.
      */
    def leftMostSourcePosition: SourcePosition = this match {
      case Pattern.Var(sp1, _, _) => sp1
      case Pattern.Lit(sp1, _, _) => sp1
      case Pattern.Tag(sp1, _, _, _) => sp1
      case Pattern.Tuple(sp1, _, _) => sp1
      case Pattern.Array(sp1, _, _) => sp1
      case Pattern.ArrayHeadSpread(sp1, _, _, _) => sp1
      case Pattern.ArrayTailSpread(sp1, _, _, _) => sp1
      case Pattern.FNil(sp1, _) => sp1
      case Pattern.FCons(hd, _, _, _) => hd.leftMostSourcePosition
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

    case class Array(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    case class ArrayTailSpread(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Pattern

    case class ArrayHeadSpread(sp1: SourcePosition, ident: Name.Ident, elms: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

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

  }

  /**
    * Choice Patterns.
    */
  sealed trait ChoicePattern

  object ChoicePattern {

    /**
      * A wildcard pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class Wild(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.ChoicePattern

    /**
      * An absent pattern.
      */
    case class Absent(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.ChoicePattern

    /**
      * A present pattern.
      *
      * @param sp1   the position of the first character in the pattern.
      * @param ident the name of the variable.
      * @param sp2   the position of the last character in the pattern.
      */
    case class Present(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.ChoicePattern

  }

  /**
    * Predicates.
    */
  sealed trait Predicate

  object Predicate {

    sealed trait Head extends ParsedAst.Predicate

    object Head {

      /**
        * Atom Predicate.
        *
        * @param sp1   the position of the first character in the predicate.
        * @param ident the qualified name of the predicate.
        * @param terms the terms of the predicate.
        * @param term  the optional lattice term (if applicable).
        * @param sp2   the position of the last character in the predicate.
        */
      case class Atom(sp1: SourcePosition, ident: Name.Ident, terms: Seq[ParsedAst.Expression], term: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Predicate.Head

      /**
        * Union Predicate.
        *
        * @param sp1 the position of the first character in the predicate.
        * @param exp the expression to evaluate and union with the current constraint set.
        * @param sp2 the position of the last character in the predicate.
        */
      case class Union(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate.Head

    }

    sealed trait Body extends ParsedAst.Predicate

    object Body {

      /**
        * Atom Predicate.
        *
        * @param sp1      the position of the first character in the predicate.
        * @param polarity the polarity of the predicate (positive/negative).
        * @param ident    the name of the predicate.
        * @param terms    the terms of the predicate.
        * @param term     the optional lattice term (if applicable).
        * @param sp2      the position of the last character in the predicate.
        */
      case class Atom(sp1: SourcePosition, polarity: Polarity, ident: Name.Ident, terms: Seq[ParsedAst.Pattern], term: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * Guard Predicate.
        *
        * @param sp1 the position of the first character in the predicate.
        * @param exp the filter expression.
        * @param sp2 the position of the last character in the predicate.
        */
      case class Guard(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * Filter Predicate.
        *
        * @param sp1   the position of the first character in the predicate.
        * @param name  the qualified name of the filter function.
        * @param terms the terms of the predicate.
        * @param sp2   the position of the last character in the predicate.
        */
      case class Filter(sp1: SourcePosition, name: Name.QName, terms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Predicate.Body

    }

  }

  /**
    * Types.
    */
  sealed trait Type

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
      * Record Type.
      *
      * @param sp1    the position of the first character in the type.
      * @param fields the sequence of field types.
      * @param rest   the optional row variable.
      * @param sp2    the position of the last character in the type.
      */
    case class Record(sp1: SourcePosition, fields: Seq[ParsedAst.RecordFieldType], rest: Option[Name.Ident], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Schema Type.
      *
      * @param sp1        the position of the first character in the type.
      * @param predicates the sequence of predicate types.
      * @param sp2        the position of the last character in the type.
      */
    case class Schema(sp1: SourcePosition, predicates: Seq[ParsedAst.PredicateType], rest: Option[Name.Ident], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Unary Impure Arrow Type.
      *
      * @param tpe1 the argument type.
      * @param tpe2 the result type.
      * @param sp2  the position of the last character in the type.
      */
    case class UnaryImpureArrow(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Unary Polymorphic Arrow Type.
      *
      * @param tpe1 the argument type.
      * @param tpe2 the result type.
      * @param eff  the optional effect.
      * @param sp2  the position of the last character in the type.
      */
    case class UnaryPolymorphicArrow(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, eff: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Impure Arrow Type.
      *
      * @param sp1     the position of the first character in the type.
      * @param tparams the arguments types.
      * @param tresult the result type.
      * @param sp2     the position of the last character in the type.
      */
    case class ImpureArrow(sp1: SourcePosition, tparams: Seq[ParsedAst.Type], tresult: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Effect Polymorphic Arrow Type.
      *
      * @param sp1     the position of the first character in the type.
      * @param tparams the arguments types.
      * @param tresult the result type.
      * @param eff     the optional effect.
      * @param sp2     the position of the last character in the type.
      */
    case class PolymorphicArrow(sp1: SourcePosition, tparams: Seq[ParsedAst.Type], tresult: ParsedAst.Type, eff: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Type

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

    /**
      * The True type constructor.
      *
      * @param sp1 the position of the first character in the type.
      * @param sp2 the position of the last character in the type.
      */
    case class True(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * The False type constructor.
      *
      * @param sp1 the position of the first character in the type.
      * @param sp2 the position of the last character in the type.
      */
    case class False(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * The Not type constructor.
      *
      * @param tpe the negated type.
      */
    case class Not(tpe: ParsedAst.Type) extends ParsedAst.Type

    /**
      * The And type constructor.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      */
    case class And(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type) extends ParsedAst.Type

    /**
      * The Or type constructor.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      */
    case class Or(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type) extends ParsedAst.Type

  }

  /**
    * Kinds.
    */
  sealed trait Kind

  object Kind {

    /**
      * The Star kind.
      */
    case class Star(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Kind

    /**
      * The Bool kind.
      */
    case class Bool(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Kind

    /**
      * The Record kind.
      */
    case class Record(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Kind

    /**
      * The Schema kind.
      */
    case class Schema(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Kind

  }

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
  case class Case(sp1: SourcePosition, ident: Name.Ident, tpe: ParsedAst.Type, sp2: SourcePosition)

  /**
    * A common super-type for a sequence of type parameters.
    */
  sealed trait TypeParams

  object TypeParams {

    /**
      * Represents an elided sequence of type parameters.
      */
    case object Elided extends TypeParams

    /**
      * Represents an explicit sequence of type parameters.
      */
    case class Explicit(tparams: List[ParsedAst.ConstrainedType]) extends TypeParams

  }

  /**
    * Documentation.
    *
    * @param sp1   the position of the first character in the comment.
    * @param lines the lines of the comment.
    * @param sp2   the position of the last character in the comment.
    */
  case class Doc(sp1: SourcePosition, lines: Seq[String], sp2: SourcePosition)

  /**
    * Context Bound.
    *
    * @param sp1     the position of the first character in the context bound.
    * @param ident   the type variable being bound
    * @param kind    the optional kind of the type variable.
    * @param classes the bounding classes.
    * @param sp2     the position of the last character in the context bound.
    */
  case class ConstrainedType(sp1: SourcePosition, ident: Name.Ident, kind: Option[ParsedAst.Kind], classes: Seq[Name.QName], sp2: SourcePosition)

  /**
    * Formal Parameter.
    *
    * @param sp1   the position of the first character in the formal parameter.
    * @param mod   the associated modifiers.
    * @param ident the name of the argument.
    * @param tpe   the optional type of the argument.
    * @param sp2   the position of the last character in the formal parameter.
    */
  case class FormalParam(sp1: SourcePosition, mod: Seq[ParsedAst.Modifier], ident: Name.Ident, tpe: Option[ParsedAst.Type], sp2: SourcePosition)

  /**
    * A catch rule consists of an identifier, a Java name, and a body expression.
    *
    * @param ident the identifier.
    * @param fqn   the fully-qualified Java name.
    * @param exp   the body expression.
    */
  case class CatchRule(ident: Name.Ident, fqn: Seq[String], exp: ParsedAst.Expression)

  /**
    * A choice pattern match rule.
    *
    * @param sp1 the position of the first character in the rule.
    * @param pat the pattern of the rule.
    * @param exp the body expression of the rule.
    * @param sp2 the position of the first character in the rule.
    */
  case class ChoiceRule(sp1: SourcePosition, pat: Seq[ParsedAst.ChoicePattern], exp: ParsedAst.Expression, sp2: SourcePosition)

  /**
    * A pattern match rule consists of a pattern, an optional pattern guard, and a body expression.
    *
    * @param pat   the pattern of the rule.
    * @param guard the optional guard of the rule.
    * @param exp   the body expression of the rule.
    */
  case class MatchRule(pat: ParsedAst.Pattern, guard: Option[ParsedAst.Expression], exp: ParsedAst.Expression)

  /**
    * A select channel rule consists of an identifier, a channel expression, and a body expression.
    *
    * @param ident the bound identifier.
    * @param chan  the channel expression of the rule.
    * @param exp   the body expression of the rule.
    */
  case class SelectChannelRule(ident: Name.Ident, chan: ParsedAst.Expression, exp: ParsedAst.Expression)

  /**
    * Modifier.
    *
    * @param sp1  the position of the first character in the modifier.
    * @param name the name of the modifier.
    * @param sp2  the position of the last character in the modifier.
    */
  case class Modifier(sp1: SourcePosition, name: String, sp2: SourcePosition)

  /**
    * A common super-type for annotations or properties.
    */
  sealed trait AnnotationOrProperty

  /**
    * Annotation.
    *
    * @param sp1   the position of the first character in the annotation.
    * @param ident the name of the annotation.
    * @param args  the arguments passed to the annotation.
    * @param sp2   the position of the last character in the annotation.
    */
  case class Annotation(sp1: SourcePosition, ident: Name.Ident, args: Option[Seq[ParsedAst.Expression]], sp2: SourcePosition) extends AnnotationOrProperty

  /**
    * String Interpolation Part.
    */
  sealed trait InterpolationPart

  object InterpolationPart {

    /**
      * Expression part of a string interpolation.
      */
    case class ExpPart(e: ParsedAst.Expression) extends InterpolationPart

    /**
      * String part of a string interpolation.
      */
    case class StrPart(s: String) extends InterpolationPart

  }

  /**
    * Jvm Operation.
    */
  sealed trait JvmOp

  object JvmOp {

    /**
      * Constructor Invocation.
      *
      * @param fqn   the fully-qualified name of the constructor.
      * @param sig   the types of the formal parameters.
      * @param ident the name given to the imported constructor.
      */
    case class Constructor(fqn: Seq[String], sig: Seq[ParsedAst.Type], ident: Name.Ident) extends JvmOp

    /**
      * Method Invocation.
      *
      * @param fqn   the fully-qualified name of the method.
      * @param sig   the types of the formal parameters.
      * @param ident the optional name given to the imported method.
      */
    case class Method(fqn: Seq[String], sig: Seq[ParsedAst.Type], ident: Option[Name.Ident]) extends JvmOp

    /**
      * Static Method Invocation.
      *
      * @param fqn   the fully-qualified name of the static method.
      * @param sig   the declared types of the formal parameters.
      * @param ident the optional name given to the imported method.
      */
    case class StaticMethod(fqn: Seq[String], sig: Seq[ParsedAst.Type], ident: Option[Name.Ident]) extends JvmOp

    /**
      * Get Object Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param ident the name given to the imported field.
      */
    case class GetField(fqn: Seq[String], ident: Name.Ident) extends JvmOp

    /**
      * Put ObjectField.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param ident the name given to the imported field.
      */
    case class PutField(fqn: Seq[String], ident: Name.Ident) extends JvmOp

    /**
      * Get Static Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param ident the name given to the imported field.
      */
    case class GetStaticField(fqn: Seq[String], ident: Name.Ident) extends JvmOp

    /**
      * Put Static Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param ident the name given to the imported field.
      */
    case class PutStaticField(fqn: Seq[String], ident: Name.Ident) extends JvmOp

  }

  /**
    * Property.
    *
    * @param sp1  the position of the first character in the property.
    * @param law  the qualified name of the law.
    * @param args the optional arguments of the property.
    * @param sp2  the position of the last character in the property.
    */
  case class Property(sp1: SourcePosition, law: Name.QName, args: Option[Seq[ParsedAst.Expression]], sp2: SourcePosition) extends AnnotationOrProperty

  /**
    * Record Operations.
    */
  sealed trait RecordOp

  object RecordOp {

    /**
      * Record Extension.
      *
      * @param sp1   the position of the first character in the operation.
      * @param label the label of the field.
      * @param exp   the value of the field.
      * @param sp2   the position of the last character in the operation.
      */
    case class Extend(sp1: SourcePosition, label: Name.Ident, exp: ParsedAst.Expression, sp2: SourcePosition) extends RecordOp

    /**
      * Record Restriction.
      *
      * @param sp1   the position of the first character in the operation.
      * @param label the label of the field.
      * @param sp2   the position of the last character in the operation.
      */
    case class Restrict(sp1: SourcePosition, label: Name.Ident, sp2: SourcePosition) extends RecordOp

    /**
      * Record Update.
      *
      * @param sp1   the position of the first character in the operation.
      * @param label the label of the field.
      * @param exp   the value of the field.
      * @param sp2   the position of the last character in the operation.
      */
    case class Update(sp1: SourcePosition, label: Name.Ident, exp: ParsedAst.Expression, sp2: SourcePosition) extends RecordOp

  }

  /**
    * Record Field Value.
    *
    * @param sp1   the position of the first character in the field.
    * @param label the label of the field.
    * @param value the value of the field.
    * @param sp2   the position of the last character in the field.
    */
  case class RecordField(sp1: SourcePosition, label: Name.Ident, value: ParsedAst.Expression, sp2: SourcePosition)

  /**
    * Record Field Type.
    *
    * @param sp1   the position of the first character in the field.
    * @param label the label of the field.
    * @param tpe   the type of the field.
    * @param sp2   the position of the last character in the field.
    */
  case class RecordFieldType(sp1: SourcePosition, label: Name.Ident, tpe: ParsedAst.Type, sp2: SourcePosition)

  /**
    * A common super-type for schema predicate types.
    */
  sealed trait PredicateType

  object PredicateType {

    /**
      * A Predicate Type that refers to a type alias.
      *
      * @param sp1   the position of the first character in the field.
      * @param qname the fully-qualified name of the type alias.
      * @param targs the optional type arguments.
      * @param sp2   the position of the last character in the field.
      */
    case class PredicateWithAlias(sp1: SourcePosition, qname: Name.QName, targs: Option[Seq[ParsedAst.Type]], sp2: SourcePosition) extends PredicateType

    /**
      * A Relational Predicate Type that is equipped wit the types of its terms.
      *
      * @param sp1  the position of the first character in the field.
      * @param name the name of the predicate symbol.
      * @param tpes the types of the terms of the predicate.
      * @param sp2  the position of the last character in the field.
      */
    case class RelPredicateWithTypes(sp1: SourcePosition, name: Name.Ident, tpes: Seq[ParsedAst.Type], sp2: SourcePosition) extends PredicateType

    /**
      * A Latticenal Predicate Type that is equipped wit the types of its terms.
      *
      * @param sp1  the position of the first character in the field.
      * @param name the name of the predicate symbol.
      * @param tpes the types of the terms of the predicate.
      * @param sp2  the position of the last character in the field.
      */
    case class LatPredicateWithTypes(sp1: SourcePosition, name: Name.Ident, tpes: Seq[ParsedAst.Type], tpe: ParsedAst.Type, sp2: SourcePosition) extends PredicateType

  }

}
