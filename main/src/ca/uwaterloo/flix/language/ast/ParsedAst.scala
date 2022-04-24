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

object ParsedAst {

  /**
    * A collection of abstract syntax trees.
    *
    * @param units      the abstract syntax trees of the parsed compilation units.
    * @param entryPoint the optional entry point.
    */
  case class Root(units: Map[Ast.Source, ParsedAst.CompilationUnit], entryPoint: Option[Symbol.DefnSym])

  /**
    * A compilation unit (i.e. a source file).
    *
    * A collection of imports and declarations.
    *
    * @param sp1   the position of the first character in the source.
    * @param uses  the uses in the abstract syntax tree.
    * @param decls the declarations in the abstract syntax tree.
    * @param sp2   the position of the last character in the source.
    */
  case class CompilationUnit(sp1: SourcePosition, uses: Seq[ParsedAst.Use], decls: Seq[ParsedAst.Declaration], sp2: SourcePosition)

  /**
    * Declarations.
    */
  sealed trait Declaration

  object Declaration {

    /**
      * Union of law declarations and sig declarations.
      */
    sealed trait LawOrSig

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
      * @param pur        the declared purity.
      * @param exp        the expression.
      * @param tconstrs   the type constraints.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Def(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, pur: Option[ParsedAst.Type], tconstrs: Seq[ParsedAst.TypeConstraint], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

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
      * @param pur        the declared purity.
      * @param tconstrs   the type constraints.
      * @param exp        the optional expression.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Sig(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, pur: Option[ParsedAst.Type], tconstrs: Seq[ParsedAst.TypeConstraint], exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Declaration with ParsedAst.Declaration.LawOrSig

    /**
      * Law Declaration.
      *
      * @param doc     the optional comment associated with the law
      * @param ann     the associated annotations.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the law.
      * @param tparams the type parameters.
      * @param fparams the value parameters.
      * @param exp     the expression.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Law(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparams: Seq[ParsedAst.FormalParam], tconstrs: Seq[ParsedAst.TypeConstraint], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration with ParsedAst.Declaration.LawOrSig

    /**
      * Effect Operation Declaration.
      *
      * @param doc        the optional comment associated with the definition.
      * @param ann        the associated annotations.
      * @param mod        the associated modifiers.
      * @param sp1        the position of the first character in the declaration.
      * @param ident      the name of the definition.
      * @param tparams    the type parameters.
      * @param fparamsOpt the formal parameters.
      * @param tpe        the declared type.
      * @param tconstrs   the type constraints.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Op(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, pur: Option[ParsedAst.Type], tconstrs: Seq[ParsedAst.TypeConstraint], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Enum Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param ann     the associated annotations.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the enum.
      * @param tparams the type parameters.
      * @param derives the derivations of the enum.
      * @param cases   the cases of the enum.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Enum(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, derives: Seq[Name.QName], cases: Seq[ParsedAst.Case], sp2: SourcePosition) extends ParsedAst.Declaration

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
    case class OpaqueType(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, derives: Seq[Name.QName], tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Declaration

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
      * Typeclass Declaration.
      *
      * @param doc          the optional comment associated with the declaration.
      * @param ann          the annotations associated with the declaration.
      * @param mod          the associated modifiers.
      * @param sp1          the position of the first character in the declaration.
      * @param ident        the name of the definition.
      * @param tparam       the type parameter.
      * @param superClasses the super classes of the class.
      * @param lawsAndSigs  the signatures and laws of the class.
      * @param sp2          the position of the last character in the declaration.
      */
    case class Class(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparam: ParsedAst.TypeParam, superClasses: Seq[ParsedAst.TypeConstraint], lawsAndSigs: Seq[ParsedAst.Declaration.LawOrSig], sp2: SourcePosition) extends ParsedAst.Declaration

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
    case class Instance(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, clazz: Name.QName, tpe: ParsedAst.Type, constraints: Seq[ParsedAst.TypeConstraint], defs: Seq[ParsedAst.Declaration.Def], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Effect Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the definition.
      * @param tparams the type parameters.
      * @param ops     the operations of the class.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Effect(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, ops: Seq[ParsedAst.Declaration.Op], sp2: SourcePosition) extends ParsedAst.Declaration

  }

  /**
    * Uses.
    */
  sealed trait Use

  object Use {

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
    * CharCodes.
    */
  sealed trait CharCode {
    val sp1: SourcePosition
    val sp2: SourcePosition
  }

  object CharCode {
    /**
      * Char literal.
      *
      * @param lit the char as a singleton string.
      */
    case class Literal(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.CharCode

    /**
      * The head of an escape sequence:
      * For standard escapes (e.g. `\t`), contains the distinguishing letter (e.g., `t`).
      * For unicode escape sequences, (e.g. `\u1234`), contains the letter `u`.
      *
      * @param seq the escape code as a singleton string.
      */
    case class Escape(sp1: SourcePosition, seq: String, sp2: SourcePosition) extends ParsedAst.CharCode
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
      * @param sp1   the position of the first character in the literal.
      * @param chars the char codes.
      * @param sp2   the position of the last character in the literal.
      */
    case class Char(sp1: SourcePosition, chars: Seq[ParsedAst.CharCode], sp2: SourcePosition) extends ParsedAst.Literal

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
      * @param sp1   the position of the first character in the literal.
      * @param chars the char codes
      * @param sp2   the position of the last character in the literal.
      */
    case class Str(sp1: SourcePosition, chars: Seq[ParsedAst.CharCode], sp2: SourcePosition) extends ParsedAst.Literal

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
      * Intrinsic Operator.
      *
      * @param sp1  the position of the first character in the expression.
      * @param op   the intrinsic.
      * @param exps the arguments.
      * @param sp2  the position of the last character in the expression.
      */
    case class Intrinsic(sp1: SourcePosition, op: Name.Ident, exps: Seq[ParsedAst.Argument], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Apply Expression (function call).
      *
      * @param lambda the lambda expression.
      * @param args   the arguments.
      * @param sp2    the position of the last character in the expression.
      */
    case class Apply(lambda: ParsedAst.Expression, args: Seq[ParsedAst.Argument], sp2: SourcePosition) extends ParsedAst.Expression

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
    case class Infix(e1: ParsedAst.Expression, name: ParsedAst.Expression, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
    case class Unary(sp1: SourcePosition, op: Operator, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Binary Expression.
      *
      * @param exp1 the left expression.
      * @param op   the binary operator.
      * @param exp2 the right expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Binary(exp1: ParsedAst.Expression, op: Operator, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param mod  the associated modifiers.
      * @param pat  the match pattern.
      * @param tpe  the optional type annotation.
      * @param exp1 the value expression.
      * @param exp2 the body expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class LetMatch(sp1: SourcePosition, mod: Seq[ParsedAst.Modifier], pat: ParsedAst.Pattern, tpe: Option[ParsedAst.Type], exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * LetRecDef Expression (let rec using def keyword).
      *
      * @param sp1     the position of the first character in the expression.
      * @param ident   the identifier of the function.
      * @param fparams the formal parameters of the function.
      * @param exp1    the function expression.
      * @param exp2    the body expression.
      * @param sp2     the position of the last character in the expression.
      */
    case class LetRecDef(sp1: SourcePosition, ident: Name.Ident, fparams: Seq[ParsedAst.FormalParam], exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * Static Region Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Static(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Scope Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param ident the name of the region.
      * @param exp   the body expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class Scope(sp1: SourcePosition, ident: Name.Ident, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param star  whether this is a choose* expression.
      * @param exps  the match expressions.
      * @param rules the rules of the pattern match.
      * @param sp2   the position of the last character in the expression.
      */
    case class Choose(sp1: SourcePosition, star: Boolean, exps: Seq[ParsedAst.Expression], rules: Seq[ChoiceRule], sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param field the field to select from the record.
      * @param sp2   the position of the last character in the expression.
      */
    case class RecordSelect(exp: ParsedAst.Expression, field: Name.Ident, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Record Select Lambda Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param field the field to select from the record.
      * @param sp2   the position of the last character in the expression.
      */
    case class RecordSelectLambda(sp1: SourcePosition, field: Name.Ident, sp2: SourcePosition) extends ParsedAst.Expression

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
      * New Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param qname the qualified name of the type.
      * @param exp   the optional region expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class New(sp1: SourcePosition, qname: Name.QName, exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArrayLit Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param exps the elements of the array.
      * @param exp  the optional region.
      * @param sp2  the position of the last character in the expression.
      */
    case class ArrayLit(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ArrayNew Expression
      *
      * @param sp1  the position of the first character in the expression.
      * @param exp1 the default value of the array elements.
      * @param exp2 the length of the array.
      * @param exp3 the optional region.
      * @param sp2  the position of the last character in the expression.
      */
    case class ArrayNew(sp1: SourcePosition, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, exp3: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param exp1 the head of the list.
      * @param sp1  the position of the first character in the :: operator.
      * @param sp2  the position of the last character in the :: operator.
      * @param exp2 the tail of the list.
      */
    case class FCons(exp1: ParsedAst.Expression, sp1: SourcePosition, sp2: SourcePosition, exp2: ParsedAst.Expression) extends ParsedAst.Expression

    /**
      * Append expression (of list).
      *
      * @param exp1 the first list.
      * @param sp1  the position of the first character in the operator @@.
      * @param sp2  the position of the last character in the operator @@.
      * @param exp2 the second list.
      */
    case class FAppend(exp1: ParsedAst.Expression, sp1: SourcePosition, sp2: SourcePosition, exp2: ParsedAst.Expression) extends ParsedAst.Expression

    /**
      * Set Expression.
      *
      * @param sp1  the position of the first character in the `Set` keyword.
      * @param sp2  the position of the last character in the `Set` keyword.
      * @param exps the elements of the set.
      */
    case class FSet(sp1: SourcePosition, sp2: SourcePosition, exps: Seq[ParsedAst.Expression]) extends ParsedAst.Expression

    /**
      * Map Expression.
      *
      * @param sp1  the position of the first character in the `Map` keyword.
      * @param sp2  the position of the last character in the `Map` keyword.
      * @param exps the (key, values) of the map.
      */
    case class FMap(sp1: SourcePosition, sp2: SourcePosition, exps: Seq[(ParsedAst.Expression, ParsedAst.Expression)]) extends ParsedAst.Expression

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
      * @param sp1  the position of the first character in the expression.
      * @param exp1 the reference.
      * @param exp2 the optional region.
      * @param sp2  the position of the last character in the expression.
      */
    case class Ref(sp1: SourcePosition, exp1: ParsedAst.Expression, exp2: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

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
      * Ascribe Expression.
      *
      * @param exp the expression.
      * @param tpe the optional type.
      * @param pur the optional purity.
      * @param sp2 the position of the last character in the expression.
      */
    case class Ascribe(exp: ParsedAst.Expression, tpe: Option[ParsedAst.Type], pur: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Cast Expression.
      *
      * @param exp the expression.
      * @param tpe the optional type.
      * @param pur the optional purity.
      * @param sp2 the position of the last character in the expression.
      */
    case class Cast(exp: ParsedAst.Expression, tpe: Option[ParsedAst.Type], pur: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Do Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param op   the effect operation.
      * @param args the arguments to the operation.
      * @param sp2  the position of the last character in the expression.
      */
    case class Do(sp1: SourcePosition, op: Name.QName, args: Seq[ParsedAst.Argument], sp2: SourcePosition) extends Expression

    /**
      * Resume Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param args the arguments to the continuation.
      * @param sp2  the position of the last character in the expression.
      */
    case class Resume(sp1: SourcePosition, args: Seq[ParsedAst.Argument], sp2: SourcePosition) extends Expression

    /**
      * Try Expression.
      *
      * @param sp1            the position of the first character in the expression.
      * @param exp            the guarded expression.
      * @param catchOrHandler the handler (catch/with) of the try expression.
      * @param sp2            the position of the last character in the expression.
      */
    case class Try(sp1: SourcePosition, exp: ParsedAst.Expression, catchOrHandler: CatchOrHandler, sp2: SourcePosition) extends ParsedAst.Expression

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
    case class FixpointConstraint(sp1: SourcePosition, con: Constraint, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Constraint Set expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param cs  the set of constraints.
      * @param sp2 the position of the last character in the expression.
      */
    case class FixpointConstraintSet(sp1: SourcePosition, cs: Seq[Constraint], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Compose expression.
      *
      * @param exp1 the first constraint expression.
      * @param exp2 the second constraint expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class FixpointCompose(exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Project-Into expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param exps the non-empty sequence of expressions to project.
      * @param into the non-empty sequence of predicate symbols to project into.
      * @param sp2  the position of the last character in the expression.
      */
    case class FixpointProjectInto(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], into: Seq[Name.Ident], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Solve-Project expression.
      *
      * @param sp1    the position of the first character in the expression.
      * @param exps   the non-empty sequence of expressions to merge and solve.
      * @param idents the (optional) non-empty sequence of predicates to project and merge out of the solution.
      * @param sp2    the position of the last character in the expression.
      */
    case class FixpointSolveWithProject(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], idents: Option[Seq[Name.Ident]], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Fixpoint Query expression.
      *
      * @param sp1      the position of the first character in the expression.
      * @param exps     the non-empty sequence of expressions to merge and solve.
      * @param selects  the expressions of the selected tuple. (the head of the pseudo-rule).
      * @param from     the predicates to select from (the body of the pseudo-rule).
      * @param whereExp the optional guard of the pseudo-rule.
      * @param sp2      the position of the last character in the expression.
      */
    case class FixpointQueryWithSelect(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], selects: Seq[ParsedAst.Expression], from: Seq[ParsedAst.Predicate.Body.Atom], whereExp: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Reify Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param t   the type to reify.
      * @param sp2 the position of the last character in the expression.
      */
    case class Reify(sp1: SourcePosition, t: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ReifyBool Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param t   the type to reify.
      * @param sp2 the position of the last character in the expression.
      */
    case class ReifyBool(sp1: SourcePosition, t: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ReifyType Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param t   the type to reify.
      * @param sp2 the position of the last character in the expression.
      */
    case class ReifyType(sp1: SourcePosition, t: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ReifyEff Expression (Will eventually be replaced by other reify expressions).
      *
      * @param sp1   the position of the first character in the expression.
      * @param exp1  the function expression on whose purity to match.
      * @param ident the name to bind the pure function to.
      * @param exp2  the then expression.
      * @param exp3  the else expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class ReifyPurity(sp1: SourcePosition, exp1: ParsedAst.Expression, ident: Name.Ident, exp2: ParsedAst.Expression, exp3: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
    * Constraint Declaration.
    *
    * @param sp1  the position of the first character in the declaration.
    * @param head the head predicate.
    * @param body the body predicates.
    * @param sp2  the position of the last character in the declaration.
    */
  case class Constraint(sp1: SourcePosition, head: ParsedAst.Predicate.Head, body: Seq[ParsedAst.Predicate.Body], sp2: SourcePosition)

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

    }

    sealed trait Body extends ParsedAst.Predicate

    object Body {

      /**
        * Atom Predicate.
        *
        * @param sp1      the position of the first character in the predicate.
        * @param polarity the polarity of the predicate (positive/negative).
        * @param fixity   the fixity of the predicate (loose/fixed).
        * @param ident    the name of the predicate.
        * @param terms    the terms of the predicate.
        * @param term     the optional lattice term (if applicable).
        * @param sp2      the position of the last character in the predicate.
        */
      case class Atom(sp1: SourcePosition, polarity: Ast.Polarity, fixity: Ast.Fixity, ident: Name.Ident, terms: Seq[ParsedAst.Pattern], term: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * Guard Predicate.
        *
        * @param sp1 the position of the first character in the predicate.
        * @param exp the filter expression.
        * @param sp2 the position of the last character in the predicate.
        */
      case class Guard(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * Loop Predicate.
        *
        * @param sp1    the position of the first character in the predicate.
        * @param idents the variable bound by the predicate.
        * @param exp    the expression to iterate over.
        * @param sp2    the position of the last character in the predicate.
        */
      case class Loop(sp1: SourcePosition, idents: Seq[Name.Ident], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate.Body

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
      * Record Row type.
      *
      * @param sp1    the position of the first character in the type.
      * @param fields the sequence of field types.
      * @param rest   the optional row variable.
      * @param sp2    the position of the last character in the type.
      */
    case class RecordRow(sp1: SourcePosition, fields: Seq[ParsedAst.RecordFieldType], rest: Option[Name.Ident], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Schema Type.
      *
      * @param sp1        the position of the first character in the type.
      * @param predicates the sequence of predicate types.
      * @param rest       the optional row variable.
      * @param sp2        the position of the last character in the type.
      */
    case class Schema(sp1: SourcePosition, predicates: Seq[ParsedAst.PredicateType], rest: Option[Name.Ident], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Schema Row Type.
      *
      * @param sp1        the position of the first character in the type.
      * @param predicates the sequence of predicate types.
      * @param rest       the optional row variable.
      * @param sp2        the position of the last character in the type.
      */
    case class SchemaRow(sp1: SourcePosition, predicates: Seq[ParsedAst.PredicateType], rest: Option[Name.Ident], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Unary Polymorphic Arrow Type.
      *
      * @param tpe1 the argument type.
      * @param tpe2 the result type.
      * @param pur  the optional purity.
      * @param sp2  the position of the last character in the type.
      */
    case class UnaryPolymorphicArrow(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, pur: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Effect Polymorphic Arrow Type.
      *
      * @param sp1     the position of the first character in the type.
      * @param tparams the arguments types.
      * @param tresult the result type.
      * @param pur     the optional purity.
      * @param sp2     the position of the last character in the type.
      */
    case class PolymorphicArrow(sp1: SourcePosition, tparams: Seq[ParsedAst.Type], tresult: ParsedAst.Type, pur: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Type

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
      * @param sp1 the position of the first character in the type.
      * @param tpe the negated type.
      * @param sp2 the position of the last character in the type.
      */
    case class Not(sp1: SourcePosition, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * The And type constructor.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class And(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * The Or type constructor.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class Or(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * Represents a union of purities.
      *
      * @param sp1  the position of the first character in the type.
      * @param purs the purities.
      * @param sp2  the position of the last character in the type.
      */
    case class Union(sp1: SourcePosition, purs: Seq[ParsedAst.Purity], sp2: SourcePosition) extends Type

    /**
      * Kind Ascription.
      *
      * @param tpe  the ascribed type.
      * @param kind the ascribed kind.
      * @param sp2  the position of the last character in the type.
      */
    case class Ascribe(tpe: ParsedAst.Type, kind: ParsedAst.Kind, sp2: SourcePosition) extends ParsedAst.Type

  }

  /**
    * Effect Set
    */
  sealed trait EffectSet

  object EffectSet {
    /**
      * Represents an effect set with a single effect.
      *
      * @param sp1 the position of the first character in the set.
      * @param eff the effect.
      * @param sp2 the position of the last character in the set.
      */
    case class Singleton(sp1: SourcePosition, eff: Effect, sp2: SourcePosition) extends EffectSet

    /**
      * Represents the empty effect set. Written as `{ }` or `{ Pure ` depending on the syntactic context.
      *
      * @param sp1 the position of the first character in the set.
      * @param sp2 the position of the last character in the set.
      */
    case class Pure(sp1: SourcePosition, sp2: SourcePosition) extends EffectSet

    /**
      * Represents a set of effects.
      *
      * @param sp1  the position of the first character in the set.
      * @param effs the effects.
      * @param sp2  the position of the last character in the set.
      */
    case class Set(sp1: SourcePosition, effs: Seq[Effect], sp2: SourcePosition) extends EffectSet
  }

  /**
    * A single effect.
    */
  sealed trait Effect

  object Effect {
    /**
      * Effect variable.
      *
      * @param sp1   the position of the first character in the effect.
      * @param ident the name of the variable.
      * @param sp2   the position of the last character in the effect.
      */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Effect

    /**
      * Represents a read of the region variables `regs`.
      *
      * @param regs the region variables that are read.
      */
    case class Read(sp1: SourcePosition, regs: Seq[Name.Ident], sp2: SourcePosition) extends ParsedAst.Effect

    /**
      * Represents a write of the region variables `regs`.
      *
      * @param regs the region variables that are written.
      */
    case class Write(sp1: SourcePosition, regs: Seq[Name.Ident], sp2: SourcePosition) extends ParsedAst.Effect

    /**
      * Represents the Impure effect.
      * This is the "top" effect, i.e., the set of all effects.
      *
      * @param sp1 the position of the first character in the effect.
      * @param sp2 the position of the last character in the effect.
      */
    case class Impure(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Effect

    /**
      * Represents a reference to an declared effect.
      *
      * @param sp1  the position of the first character in the effect.
      * @param name the fully qualified name of the effect.
      * @param sp2  the position of the last character in the effect.
      */
    case class Eff(sp1: SourcePosition, name: Name.QName, sp2: SourcePosition) extends ParsedAst.Effect

    /**
      * Represents the complement of an effect set.
      *
      * @param sp1 the position of the first character in the effect.
      * @param eff the complemented effect.
      * @param sp2 the position of the last character in the effect.
      */
    case class Complement(sp1: SourcePosition, eff: ParsedAst.Effect, sp2: SourcePosition) extends ParsedAst.Effect

    /**
      * Represents the union of effect sets.
      *
      * @param eff1 the first effect.
      * @param effs the other effects.
      */
    case class Union(eff1: ParsedAst.Effect, effs: Seq[ParsedAst.Effect]) extends ParsedAst.Effect

    /**
      * Represents the intersection of effect sets.
      *
      * @param eff1 the first effect.
      * @param effs the other effects.
      */
    case class Intersection(eff1: ParsedAst.Effect, effs: Seq[ParsedAst.Effect]) extends ParsedAst.Effect

    /**
      * Represents the difference of effect sets.
      *
      * @param eff1 the first effect.
      * @param effs the other effects.
      */
    case class Difference(eff1: ParsedAst.Effect, effs: Seq[ParsedAst.Effect]) extends ParsedAst.Effect
  }

  /**
    * Represents a purity.
    */
  sealed trait Purity

  object Purity {

    /**
      * Represents a purity variable.
      *
      * @param sp1   the position of the first character in the type.
      * @param ident the variable name.
      * @param sp2   the position of the last character in the type.
      */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Purity

    /**
      * Represents a read of the region variables `idents`.
      *
      * @param idents the region variables that are read.
      */
    case class Read(idents: Seq[Name.Ident]) extends ParsedAst.Purity

    /**
      * Represents a write of the region variables `idents`.
      *
      * @param idents the region variables that are written.
      */
    case class Write(idents: Seq[Name.Ident]) extends ParsedAst.Purity

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
      * The Region kind.
      */
    case class Region(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Kind

    /**
      * The Record Row kind.
      */
    case class RecordRow(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Kind

    /**
      * The Schema Row kind.
      */
    case class SchemaRow(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Kind

    /**
      * The Predicate kind.
      */
    case class Predicate(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Kind

    /**
      * The Arrow kind.
      */
    case class Arrow(k1: ParsedAst.Kind, k2: ParsedAst.Kind, sp2: SourcePosition) extends ParsedAst.Kind

  }

  /**
    * An argument to a function application.
    */
  sealed trait Argument

  object Argument {

    /**
      * A named argument.
      *
      * @param name the optional argument name.
      * @param exp  the value of the argument.
      * @param sp2  the position of the last character in the argument.
      */
    case class Named(name: Name.Ident, exp: ParsedAst.Expression, sp2: SourcePosition) extends Argument

    /**
      * An unnamed argument.
      *
      * @param exp the value of the argument.
      */
    case class Unnamed(exp: ParsedAst.Expression) extends Argument
  }

  /**
    * Operator.
    *
    * @param sp1 the position of the first character in the operator.
    * @param op  the operator.
    * @param sp2 the position of the last character in the operator.
    */
  case class Operator(sp1: SourcePosition, op: String, sp2: SourcePosition)

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
    case class Explicit(tparams: List[ParsedAst.TypeParam]) extends TypeParams

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
    * A single type parameter, with optional kind and constraint.
    *
    * @param sp1   the position of the first character in the type parameter.
    * @param ident the type variable being bound
    * @param kind  the optional kind of the type variable.
    * @param sp2   the position of the last character in the type parameter.
    */
  case class TypeParam(sp1: SourcePosition, ident: Name.Ident, kind: Option[ParsedAst.Kind], sp2: SourcePosition)

  /**
    * A type constraint.
    *
    * @param sp1    the position of the first character in the type constraint.
    * @param clazz  name of the class.
    * @param tparam the name of the constrained type.
    * @param sp2    the position of the last character in the type constraint.
    */
  case class TypeConstraint(sp1: SourcePosition, clazz: Name.QName, tparam: ParsedAst.Type, sp2: SourcePosition)

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
    * Effect handler rule.
    *
    * @param name    the operation name.
    * @param fparams the operation parameters.
    * @param exp     the body expression.
    */
  case class HandlerRule(name: Name.Ident, fparams: Seq[FormalParam], exp: ParsedAst.Expression)

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
    * Annotation.
    *
    * @param sp1   the position of the first character in the annotation.
    * @param ident the name of the annotation.
    * @param args  the arguments passed to the annotation.
    * @param sp2   the position of the last character in the annotation.
    */
  case class Annotation(sp1: SourcePosition, ident: Name.Ident, args: Option[Seq[ParsedAst.Argument]], sp2: SourcePosition)

  /**
    * An enum representing the handler of a `try` expression.
    */
  sealed trait CatchOrHandler

  object CatchOrHandler {
    /**
      * A `catch` block for handling Java exceptions.
      *
      * @param rules the catch rules.
      */
    case class Catch(rules: Seq[ParsedAst.CatchRule]) extends CatchOrHandler

    /**
      * A `with` block for handling Flix effects.
      *
      * @param eff   the effect to be handled.
      * @param rules the handler rules.
      */
    case class Handler(eff: Name.QName, rules: Option[Seq[ParsedAst.HandlerRule]]) extends CatchOrHandler
  }

  /**
    * String Interpolation Part.
    */
  sealed trait InterpolationPart

  object InterpolationPart {

    /**
      * Expression part of a string interpolation.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class ExpPart(sp1: SourcePosition, exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends InterpolationPart

    /**
      * String part of a string interpolation.
      *
      * @param sp1   the position of the first character in the string.
      * @param chars the char codes.
      * @param sp2   the position of the last character in the string.
      */
    case class StrPart(sp1: SourcePosition, chars: Seq[ParsedAst.CharCode], sp2: SourcePosition) extends InterpolationPart

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
      * @param tpe   the return type of the constructor.
      * @param pur   the purity of the constructor.
      * @param ident the name given to the imported constructor.
      */
    case class Constructor(fqn: Seq[String], sig: Seq[ParsedAst.Type], tpe: Type, pur: Type, ident: Name.Ident) extends JvmOp

    /**
      * Method Invocation.
      *
      * @param fqn   the fully-qualified name of the method.
      * @param sig   the types of the formal parameters.
      * @param tpe   the return type of the imported method.
      * @param pur   the purity of the imported method.
      * @param ident the optional name given to the imported method.
      */
    case class Method(fqn: Seq[String], sig: Seq[ParsedAst.Type], tpe: Type, pur: Type, ident: Option[Name.Ident]) extends JvmOp

    /**
      * Static Method Invocation.
      *
      * @param fqn   the fully-qualified name of the static method.
      * @param sig   the declared types of the formal parameters.
      * @param tpe   the return type of the imported method.
      * @param pur   the purity of the imported method.
      * @param ident the optional name given to the imported method.
      */
    case class StaticMethod(fqn: Seq[String], sig: Seq[ParsedAst.Type], tpe: Type, pur: Type, ident: Option[Name.Ident]) extends JvmOp

    /**
      * Get Object Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param tpe   the return type of the generated function.
      * @param pur   the purity of the generated function.
      * @param ident the name given to the imported field.
      */
    case class GetField(fqn: Seq[String], tpe: Type, pur: Type, ident: Name.Ident) extends JvmOp

    /**
      * Put ObjectField.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param tpe   the return type of the generated function.
      * @param pur   the purity of the generated function.
      * @param ident the name given to the imported field.
      */
    case class PutField(fqn: Seq[String], tpe: Type, pur: Type, ident: Name.Ident) extends JvmOp

    /**
      * Get Static Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param tpe   the return type of the generated function.
      * @param pur   the purity of the generated function.
      * @param ident the name given to the imported field.
      */
    case class GetStaticField(fqn: Seq[String], tpe: Type, pur: Type, ident: Name.Ident) extends JvmOp

    /**
      * Put Static Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param tpe   the return type of the generated function.
      * @param pur   the purity of the generated function.
      * @param ident the name given to the imported field.
      */
    case class PutStaticField(fqn: Seq[String], tpe: Type, pur: Type, ident: Name.Ident) extends JvmOp

  }

  /**
    * Record Operations.
    */
  sealed trait RecordOp

  object RecordOp {

    /**
      * Record Extension.
      *
      * @param sp1   the position of the first character in the operation.
      * @param field the field of the field.
      * @param exp   the value of the field.
      * @param sp2   the position of the last character in the operation.
      */
    case class Extend(sp1: SourcePosition, field: Name.Ident, exp: ParsedAst.Expression, sp2: SourcePosition) extends RecordOp

    /**
      * Record Restriction.
      *
      * @param sp1   the position of the first character in the operation.
      * @param field the field of the field.
      * @param sp2   the position of the last character in the operation.
      */
    case class Restrict(sp1: SourcePosition, field: Name.Ident, sp2: SourcePosition) extends RecordOp

    /**
      * Record Update.
      *
      * @param sp1   the position of the first character in the operation.
      * @param field the field of the field.
      * @param exp   the value of the field.
      * @param sp2   the position of the last character in the operation.
      */
    case class Update(sp1: SourcePosition, field: Name.Ident, exp: ParsedAst.Expression, sp2: SourcePosition) extends RecordOp

  }

  /**
    * Record Field Value.
    *
    * @param sp1   the position of the first character in the field.
    * @param field the field of the field.
    * @param value the value of the field.
    * @param sp2   the position of the last character in the field.
    */
  case class RecordField(sp1: SourcePosition, field: Name.Ident, value: ParsedAst.Expression, sp2: SourcePosition)

  /**
    * Record Field Type.
    *
    * @param sp1   the position of the first character in the field.
    * @param field the field of the field.
    * @param tpe   the type of the field.
    * @param sp2   the position of the last character in the field.
    */
  case class RecordFieldType(sp1: SourcePosition, field: Name.Ident, tpe: ParsedAst.Type, sp2: SourcePosition)

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
