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

import ca.uwaterloo.flix.util.collection.MultiMap

object ParsedAst {

  /**
    * Represents the empty AST.
    */
  val empty: Root = Root(Map.empty, None, MultiMap.empty)

  /**
    * A collection of abstract syntax trees.
    *
    * @param units      the abstract syntax trees of the parsed compilation units.
    * @param entryPoint the optional entry point.
    */
  case class Root(units: Map[Ast.Source, ParsedAst.CompilationUnit], entryPoint: Option[Symbol.DefnSym], names: MultiMap[List[String], String])

  /**
    * A compilation unit (i.e. a source file).
    *
    * A collection of imports and declarations.
    *
    * @param sp1           the position of the first character in the source.
    * @param usesOrImports the uses in the abstract syntax tree.
    * @param decls         the declarations in the abstract syntax tree.
    * @param sp2           the position of the last character in the source.
    */
  case class CompilationUnit(sp1: SourcePosition, usesOrImports: Seq[ParsedAst.UseOrImport], decls: Seq[ParsedAst.Declaration], sp2: SourcePosition)

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
    case class Namespace(sp1: SourcePosition, name: Name.NName, uses: Seq[ParsedAst.UseOrImport], decls: Seq[ParsedAst.Declaration], sp2: SourcePosition) extends ParsedAst.Declaration

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
      * @param eff        the declared effect.
      * @param exp        the expression.
      * @param tconstrs   the type constraints.
      * @param econstrs   the equality constraints.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Def(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Type], tconstrs: Seq[ParsedAst.TypeConstraint], econstrs: Seq[ParsedAst.EqualityConstraint], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Declaration

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
      * @param eff        the declared effect.
      * @param tconstrs   the type constraints.
      * @param exp        the optional expression.
      * @param sp2        the position of the last character in the declaration.
      */
    case class Sig(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Type], tconstrs: Seq[ParsedAst.TypeConstraint], exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Declaration.LawOrSig

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
    case class Op(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, fparamsOpt: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Type], tconstrs: Seq[ParsedAst.TypeConstraint], sp2: SourcePosition)

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
    case class Enum(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, tpe: Option[ParsedAst.Type], derives: Derivations, cases: Option[Seq[ParsedAst.Case]], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Restrictable Enum Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param ann     the associated annotations.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the enum.
      * @param index   the type parameter the describes the restriction of tags.
      * @param tparams the type parameters.
      * @param derives the derivations of the enum.
      * @param cases   the cases of the enum.
      * @param sp2     the position of the last character in the declaration.
      */
    case class RestrictableEnum(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, index: ParsedAst.TypeParam, tparams: ParsedAst.TypeParams, tpe: Option[ParsedAst.Type], derives: Derivations, cases: Option[Seq[ParsedAst.RestrictableCase]], sp2: SourcePosition) extends ParsedAst.Declaration

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
      * Associated Type Signature Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the type.
      * @param tparams the type parameters of the type.
      * @param kind    the kind of the type.
      * @param sp2     the position of the last character in the declaration.
      */
    case class AssocTypeSig(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, kind: Option[ParsedAst.Kind], sp2: SourcePosition)

    /**
      * Associated Type Declaration.
      *
      * @param doc   the optional comment associated with the declaration.
      * @param mod   the associated modifiers.
      * @param sp1   the position of the first character in the declaration.
      * @param ident the name of the type.
      * @param args  the arguments of the type.
      * @param tpe   the type of the type.
      * @param sp2   the position of the last character in the declaration.
      */
    case class AssocTypeDef(doc: ParsedAst.Doc, mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, args: Option[Seq[ParsedAst.Type]], tpe: ParsedAst.Type, sp2: SourcePosition)

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
      * @param assocs       the associated types
      * @param lawsAndSigs  the signatures and laws of the class.
      * @param sp2          the position of the last character in the declaration.
      */
    case class Class(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparam: ParsedAst.TypeParam, superClasses: Seq[ParsedAst.TypeConstraint], assocs: Seq[ParsedAst.Declaration.AssocTypeSig], lawsAndSigs: Seq[ParsedAst.Declaration.LawOrSig], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Typeclass instance.
      *
      * @param doc    the optional comment associated with the declaration.
      * @param ann    the annotations associated with the declaration.
      * @param mod    the associated modifiers.
      * @param sp1    the position of the first character in the declaration.
      * @param clazz  the name of the class.
      * @param tpe    the type of the instance.
      * @param assocs the associated types
      * @param defs   the definitions of the instance.
      * @param sp2    the position of the last character in the declaration.
      */
    case class Instance(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, clazz: Name.QName, tpe: ParsedAst.Type, constraints: Seq[ParsedAst.TypeConstraint], assocs: Seq[ParsedAst.Declaration.AssocTypeDef], defs: Seq[ParsedAst.Declaration.Def], sp2: SourcePosition) extends ParsedAst.Declaration

    /**
      * Effect Declaration.
      *
      * @param doc     the optional comment associated with the declaration.
      * @param ann     the annotations associated with the declaration.
      * @param mod     the associated modifiers.
      * @param sp1     the position of the first character in the declaration.
      * @param ident   the name of the definition.
      * @param tparams the type parameters.
      * @param ops     the operations of the class.
      * @param sp2     the position of the last character in the declaration.
      */
    case class Effect(doc: ParsedAst.Doc, ann: Seq[ParsedAst.Annotation], mod: Seq[ParsedAst.Modifier], sp1: SourcePosition, ident: Name.Ident, tparams: ParsedAst.TypeParams, ops: Seq[ParsedAst.Declaration.Op], sp2: SourcePosition) extends ParsedAst.Declaration

  }

  /**
    * A common super-type for uses or imports.
    */
  sealed trait UseOrImport

  /**
    * Uses.
    */
  sealed trait Use extends UseOrImport

  object Use {

    /**
      * A use of a single name from a namespace.
      *
      * @param sp1   the position of the first character.
      * @param qname the qualified name.
      * @param sp2   the position of the last character.
      */
    case class UseOne(sp1: SourcePosition, qname: Name.QName, sp2: SourcePosition) extends ParsedAst.Use

    /**
      * A use of multiple names from a namespace.
      *
      * @param sp1   the position of the first character.
      * @param nname the namespace.
      * @param names the names.
      * @param sp2   the position of the last character.
      */
    case class UseMany(sp1: SourcePosition, nname: Name.NName, names: Seq[ParsedAst.Use.NameAndAlias], sp2: SourcePosition) extends ParsedAst.Use

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
    * Imports.
    */
  sealed trait Import extends UseOrImport

  object Imports {

    /**
      * An import of a single class or interface
      *
      * @param sp1  the position of the first character.
      * @param name the Java class or interface name.
      * @param sp2  the position of the last character.
      */
    case class ImportOne(sp1: SourcePosition, name: Name.JavaName, sp2: SourcePosition) extends ParsedAst.Import

    /**
      * An import of multiple classes or interfaces from a single package
      *
      * @param sp1 the position of the first character.
      * @param pkg the Java package name.
      * @param ids the names of the classes or interfaces.
      * @param sp2 the position of the last character.
      */
    case class ImportMany(sp1: SourcePosition, pkg: Name.JavaName, ids: Seq[NameAndAlias], sp2: SourcePosition) extends ParsedAst.Import

    /**
      * A name with an optional alias
      *
      * @param sp1   the position of the first character.
      * @param name  the name.
      * @param alias the optional alias.
      * @param sp2   the position of the last character.
      */
    case class NameAndAlias(sp1: SourcePosition, name: String, alias: Option[Name.Ident], sp2: SourcePosition)
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
      * @param sign   the sign.
      * @param before the digits before the decimal point.
      * @param after  the digits after the decimal point.
      * @param sp2    the position of the last character in the literal.
      */
    case class Float32(sp1: SourcePosition, sign: String, before: String, after: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Float64 Literal (64-bit floating-point number).
      *
      * @param sp1    the position of the first character in the literal.
      * @param sign   the sign.
      * @param before the digits before the decimal point.
      * @param after  the digits after the decimal point.
      * @param sp2    the position of the last character in the literal.
      */
    case class Float64(sp1: SourcePosition, sign: String, before: String, after: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * BigDecimal Literal (arbitrary sized floating-point number).
      *
      * @param sp1    the position of the first character in the literal.
      * @param sign   the sign.
      * @param before the digits before the decimal point.
      * @param after  the digits after the decimal point.
      * @param sp2    the position of the last character in the literal.
      */
    case class BigDecimal(sp1: SourcePosition, sign: String, before: String, after: Option[String], power: Option[String], sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int8 Literal (signed 8-bit integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign.
      * @param radix the radix of the literal.
      * @param lit   the int8 literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class Int8(sp1: SourcePosition, sign: String, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int16 Literal (signed 16-bit integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign.
      * @param radix the radix of the literal.
      * @param lit   the int16 literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class Int16(sp1: SourcePosition, sign: String, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int32 Literal (signed 32-bit integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign.
      * @param radix the radix of the literal.
      * @param lit   the int32 literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class Int32(sp1: SourcePosition, sign: String, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Int64 Literal (signed 64-bit integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign.
      * @param radix the radix of the literal.
      * @param lit   the int64 literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class Int64(sp1: SourcePosition, sign: String, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * BigInt Literal (arbitrary sized integer).
      *
      * @param sp1   the position of the first character in the literal.
      * @param sign  the sign.
      * @param radix the radix of the literal.
      * @param lit   the big int literal.
      * @param sp2   the position of the last character in the literal.
      */
    case class BigInt(sp1: SourcePosition, sign: String, radix: Int, lit: String, sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * String Literal.
      *
      * @param sp1   the position of the first character in the literal.
      * @param chars the char codes
      * @param sp2   the position of the last character in the literal.
      */
    case class Str(sp1: SourcePosition, chars: Seq[ParsedAst.CharCode], sp2: SourcePosition) extends ParsedAst.Literal

    /**
      * Regex Pattern Literal.
      *
      * @param sp1   the position of the first character in the literal.
      * @param chars the regular expression pattern
      * @param sp2   the position of the last character in the literal.
      */
    case class Regex(sp1: SourcePosition, chars: Seq[ParsedAst.CharCode], sp2: SourcePosition) extends ParsedAst.Literal

  }

  /**
    * Expressions.
    */
  sealed trait Expression

  object Expression {

    /**
      * Qualified Name Expression (reference expression).
      *
      * @param sp1  the position of the first character in the expression.
      * @param name the name.
      * @param sp2  the position of the last character in the expression.
      */
    case class QName(sp1: SourcePosition, name: Name.QName, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * An Open Qualified Name Expression (This opens the type of restrictable tags) (reference expression).
      *
      * @param sp1  the position of the first character in the expression.
      * @param name the name.
      * @param sp2  the position of the last character in the expression.
      */
    case class Open(sp1: SourcePosition, name: Name.QName, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * An Open Qualified Name Expression (This opens the type of restrictable tags) (reference expression).
      *
      * @param sp1  the position of the first character in the expression.
      * @param name the name.
      * @param exp  the body expression
      * @param sp2  the position of the last character in the expression.
      */
    case class OpenAs(sp1: SourcePosition, name: Name.QName, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Hole Expression.
      *
      * @param sp1   the position of the first character in the expression
      * @param ident the optional name of the hole.
      * @param sp2   the position of the last character in the expression.
      */
    case class Hole(sp1: SourcePosition, ident: Option[Name.Ident], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Ident with Hole Expression.
      *
      * @param ident the ident of the expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class HolyName(ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param exp  the lambda expression.
      * @param args the arguments.
      * @param sp2  the position of the last character in the expression.
      */
    case class Apply(exp: ParsedAst.Expression, args: Seq[ParsedAst.Argument], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Infix Apply.
      *
      * Replaced with Apply by Weeder.
      *
      * @param exp1 the first argument expression.
      * @param exp2 the name of the function.
      * @param exp3 the second argument expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Infix(exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, exp3: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * Discard expression. Evaluates the inner expression and throws away its result. Evaluates to Unit.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Discard(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * LetRecDef Expression (let rec using def keyword).
      *
      * @param sp1     the position of the first character in the expression.
      * @param ident   the identifier of the function.
      * @param fparams the formal parameters of the function.
      * @param exp1    the function expression.
      * @param exp2    the body expression.
      * @param sp2     the position of the last character in the expression.
      */
    case class LetRecDef(sp1: SourcePosition, ident: Name.Ident, fparams: Seq[ParsedAst.FormalParam], typeAndEff: Option[(ParsedAst.Type, Option[ParsedAst.Type])], exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * NewObject (create an anonymous object which implements a Java interface or extends a Java class).
      *
      * @param sp1     the position of the first character in the expression.
      * @param tpe     the class or interface (specified as a JVM type or type alias).
      * @param methods implementations of the methods within the Java interface or class
      * @param sp2     the position of the last character in the expression.
      */
    case class NewObject(sp1: SourcePosition, tpe: ParsedAst.Type, methods: Seq[JvmMethod], sp2: SourcePosition) extends ParsedAst.Expression

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
      * Type Match Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param exp   the value expression.
      * @param rules the rules of the type match.
      * @param sp2   the position of the last character in the expression.
      */
    case class TypeMatch(sp1: SourcePosition, exp: ParsedAst.Expression, rules: Seq[ParsedAst.TypeMatchRule], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Relational Choose Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param star  whether this is a relational_choose* expression.
      * @param exps  the match expressions.
      * @param rules the rules of the pattern match.
      * @param sp2   the position of the last character in the expression.
      */
    case class RelationalChoose(sp1: SourcePosition, star: Boolean, exps: Seq[ParsedAst.Expression], rules: Seq[RelationalChooseRule], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Restrictable Choose Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param star  whether this is a choose* expression.
      * @param exp   the match expressions.
      * @param rules the rules of the pattern match.
      * @param sp2   the position of the last character in the expression.
      */
    case class RestrictableChoose(sp1: SourcePosition, star: Boolean, exp: ParsedAst.Expression, rules: Seq[MatchRule], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Applicative For Expression (`forA (...) yield`) .
      *
      * @param sp1   the position of the first character in the expression.
      * @param frags the for-fragments, specifically [[ForFragment.Generator]].
      * @param exp   the yield-expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class ApplicativeFor(sp1: SourcePosition, frags: Seq[ForFragment.Generator], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ForEach Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param frags the for-fragments.
      * @param exp   the body expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class ForEach(sp1: SourcePosition, frags: Seq[ForFragment], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * MonadicFor Expression (`for (...) yield`).
      *
      * @param sp1   the position of the first character in the expression.
      * @param frags the for-fragments.
      * @param exp   the yield-expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class MonadicFor(sp1: SourcePosition, frags: Seq[ForFragment], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ForEachYield Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param frags the for-fragments.
      * @param exp   the yield-expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class ForEachYield(sp1: SourcePosition, frags: Seq[ForFragment], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Tuple Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the expression.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Argument], sp2: SourcePosition) extends ParsedAst.Expression

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
      * Record Operation Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param ops the sequence of record operations.
      * @param exp the base record to apply the operation to.
      * @param sp2 the position of the last character in the expression.
      */
    case class RecordOperation(sp1: SourcePosition, ops: Seq[ParsedAst.RecordOp], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Array Literal expression.
      *
      * @param sp1  the position of the first character in the `Array` keyword.
      * @param exps the elements of the array.
      * @param exp  the region of the array.
      * @param sp2  the position of the last character in the expression.
      */
    case class ArrayLit(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Vector Literal expression.
      *
      * @param sp1  the position of the first character in the `Vector` keyword.
      * @param exps the elements of the vector.
      * @param sp2  the position of the last character in the expression.
      */
    case class VectorLit(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

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
      * List Literal Expression.
      *
      * @param sp1  the position of the first character in the `List` keyword.
      * @param sp2  the position of the last character in the `List` keyword.
      * @param exps the elements of the list.
      */
    case class ListLit(sp1: SourcePosition, sp2: SourcePosition, exps: Seq[ParsedAst.Expression]) extends ParsedAst.Expression

    /**
      * Set Literal Expression.
      *
      * @param sp1  the position of the first character in the `Set` keyword.
      * @param sp2  the position of the last character in the `Set` keyword.
      * @param exps the elements of the set.
      */
    case class SetLit(sp1: SourcePosition, sp2: SourcePosition, exps: Seq[ParsedAst.Expression]) extends ParsedAst.Expression

    /**
      * Map Literal Expression.
      *
      * @param sp1  the position of the first character in the `Map` keyword.
      * @param sp2  the position of the last character in the `Map` keyword.
      * @param exps the (key, values) of the map.
      */
    case class MapLit(sp1: SourcePosition, sp2: SourcePosition, exps: Seq[(ParsedAst.Expression, ParsedAst.Expression)]) extends ParsedAst.Expression

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
      * @param exp2 the region.
      * @param sp2  the position of the last character in the expression.
      */
    case class Ref(sp1: SourcePosition, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param eff the optional effect.
      * @param sp2 the position of the last character in the expression.
      */
    case class Ascribe(exp: ParsedAst.Expression, tpe: ParsedAst.Type, eff: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * InstanceOf expression.
      *
      * @param exp  the expression.
      * @param name the Java class or interface name.
      * @param sp2  the position of the last character in the expression.
      */
    case class InstanceOf(exp: ParsedAst.Expression, name: Name.JavaName, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Checked Type Cast expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class CheckedTypeCast(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Checked Effect Cast expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class CheckedEffectCast(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Unchecked Cast Expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param tpe the optional type.
      * @param eff the optional effect.
      * @param sp2 the position of the last character in the expression.
      */
    case class UncheckedCast(sp1: SourcePosition, exp: ParsedAst.Expression, tpe: ParsedAst.Type, eff: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Unchecked Masking Cast expression
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression to mask.
      * @param sp2 the position of the last character in the expression.
      */
    case class UncheckedMaskingCast(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Without Expression.
      *
      * @param exp  the expression.
      * @param effs the effects.
      * @param sp2  the position of the last character in the expression.
      */
    case class Without(exp: ParsedAst.Expression, effs: Seq[Name.QName], sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param sp1 the position of the first character in the expression.
      * @param arg the argument to the continuation.
      * @param sp2 the position of the last character in the expression.
      */
    case class Resume(sp1: SourcePosition, arg: ParsedAst.Argument, sp2: SourcePosition) extends Expression

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
      * SelectChannel Expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param rules the rules of the select expression.
      * @param exp   the default of the select expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class SelectChannel(sp1: SourcePosition, rules: Seq[SelectChannelRule], exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Spawn Expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param exp1 the spawned expression (i.e. the expression to run in a new thread).
      * @param exp2 the region expression (i.e. the region in which to create the thread).
      * @param sp2  the position of the last character in the expression.
      */
    case class Spawn(sp1: SourcePosition, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * ParYield expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param frags the [[ParYieldFragment]] fragments i.e. `a <- exp`.
      * @param exp   the yield expression.
      * @param sp2   the position of the last character in the expression.
      */
    case class ParYield(sp1: SourcePosition, frags: Seq[ParsedAst.ParYieldFragment], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      * Fixpoint Lambda expression.
      *
      * @param sp1     the position of the first character in the expression.
      * @param pparams the predicate parameters.
      * @param exp     the constraint expression.
      * @param sp2     the position of the last character in the expression.
      */
    case class FixpointLambda(sp1: SourcePosition, pparams: Seq[ParsedAst.PredicateParam], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
    case class FixpointInjectInto(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], into: Seq[Name.Ident], sp2: SourcePosition) extends ParsedAst.Expression

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
      * @param sp1  the position of the first character in the expression.
      * @param exps the non-empty sequence of expressions to merge and solve.
      * @param exp1 the expressions of the selected tuple. (the head of the pseudo-rule).
      * @param from the predicates to select from (the body of the pseudo-rule).
      * @param exp2 the optional guard of the pseudo-rule.
      * @param sp2  the position of the last character in the expression.
      */
    case class FixpointQueryWithSelect(sp1: SourcePosition, exps: Seq[ParsedAst.Expression], exp1: Seq[ParsedAst.Expression], from: Seq[ParsedAst.Predicate.Body.Atom], exp2: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression

    /**
      * Debug expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param kind the debug kind.
      * @param exp  the expression to print.
      * @param sp2  the position of the last character in the expression.
      */
    case class Debug(sp1: SourcePosition, kind: ParsedAst.DebugKind, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression

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
      case Pattern.FCons(hd, _, _, _) => hd.leftMostSourcePosition
      case Pattern.Record(sp1, _, _, _) => sp1
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
      * Record Pattern
      *
      * @param sp1    the position of the first character in the pattern.
      * @param fields list of [[RecordFieldPattern]].
      * @param rest   optional record extension pattern `| r`.
      * @param sp2    the position of the last character in the pattern.
      */
    case class Record(sp1: SourcePosition, fields: Seq[RecordFieldPattern], rest: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * Represents a pattern for a field of a record.
      * `field {= pattern}`
      *
      * @param sp1   the position of the first character in the pattern.
      * @param field the field the pattern refers to.
      * @param pat   optional pattern.
      * @param sp2   the position of the last character in the pattern.
      */
    case class RecordFieldPattern(sp1: SourcePosition, field: Name.Ident, pat: Option[Pattern], sp2: SourcePosition)

  }

  /**
    * Relational Choice Patterns.
    */
  sealed trait RelationalChoosePattern

  object RelationalChoosePattern {

    /**
      * A wildcard pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class Wild(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.RelationalChoosePattern

    /**
      * An absent pattern.
      */
    case class Absent(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.RelationalChoosePattern

    /**
      * A present pattern.
      *
      * @param sp1   the position of the first character in the pattern.
      * @param ident the name of the variable.
      * @param sp2   the position of the last character in the pattern.
      */
    case class Present(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.RelationalChoosePattern

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
        * @param exps  the terms of the predicate.
        * @param exp1  the optional lattice term (if applicable).
        * @param sp2   the position of the last character in the predicate.
        */
      case class Atom(sp1: SourcePosition, ident: Name.Ident, exps: Seq[ParsedAst.Expression], exp1: Option[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Predicate.Head

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
        * Functional Predicate.
        *
        * @param sp1    the position of the first character in the predicate.
        * @param idents the out variables of the predicate.
        * @param exp    the expression to iterate over.
        * @param sp2    the position of the last character in the predicate.
        */
      case class Functional(sp1: SourcePosition, idents: Seq[Name.Ident], exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate.Body

      /**
        * Guard Predicate.
        *
        * @param sp1 the position of the first character in the predicate.
        * @param exp the filter expression.
        * @param sp2 the position of the last character in the predicate.
        */
      case class Guard(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Predicate.Body

    }

  }

  /**
    * Types.
    */
  sealed trait Type {
    def sp2: SourcePosition
  }

  object Type {

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
      * @param eff  the optional effect.
      * @param sp2  the position of the last character in the type.
      */
    case class UnaryPolymorphicArrow(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, eff: Option[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Type

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
    case class Native(sp1: SourcePosition, fqn: Name.JavaName, sp2: SourcePosition) extends ParsedAst.Type

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
      * The Pure type constructor.
      *
      * @param sp1 the position of the first character in the type.
      * @param sp2 the position of the last character in the type.
      */
    case class Pure(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * The Impure type constructor.
      *
      * @param sp1 the position of the first character in the type.
      * @param sp2 the position of the last character in the type.
      */
    case class Impure(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing an effect set.
      */
    case class EffectSet(sp1: SourcePosition, tpes: Seq[ParsedAst.Type], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a union of two effect set formulas.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class Union(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing an intersection of two effect set formulas.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class Intersection(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a difference of two effect set formulas.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class Difference(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing the complement of a effect set formula.
      *
      * @param tpe the complemented type.
      * @param sp2 the position of the last character in the type.
      */
    case class Complement(sp1: SourcePosition, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a logical negation.
      *
      * @param tpe the complemented type.
      * @param sp2 the position of the last character in the type.
      */
    case class Not(sp1: SourcePosition, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a logical conjunction.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class And(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a logical disjunction.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class Or(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a logical exclusive or.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class Xor(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a case set.
      *
      * @param sp1   the position of the first character in the type.
      * @param cases the case constants.
      * @param sp2   the position of the last character in the type.
      */
    case class CaseSet(sp1: SourcePosition, cases: Seq[Name.QName], sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a union of two case set formulas.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class CaseUnion(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing an intersection of two case set formulas.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class CaseIntersection(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing a difference of two case set formulas.
      *
      * @param tpe1 the 1st type.
      * @param tpe2 the 2nd type.
      * @param sp2  the position of the last character in the type.
      */
    case class CaseDifference(tpe1: ParsedAst.Type, tpe2: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

    /**
      * A type representing the complement of a case set formula.
      *
      * @param tpe the complemented type.
      * @param sp2 the position of the last character in the type.
      */
    case class CaseComplement(sp1: SourcePosition, tpe: ParsedAst.Type, sp2: SourcePosition) extends ParsedAst.Type

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
    * Kinds.
    */
  sealed trait Kind

  object Kind {

    /**
      * A non-builtin kind.
      */
    case class QName(sp1: SourcePosition, qname: Name.QName, sp2: SourcePosition) extends ParsedAst.Kind

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
  case class Case(sp1: SourcePosition, ident: Name.Ident, tpe: Option[ParsedAst.Type], sp2: SourcePosition)

  /**
    * Restrictable Case (member of a restrictable enum).
    *
    * @param sp1   the position of the first character in the case declaration.
    * @param ident the name of the declared tag.
    * @param tpe   the type of the declared tag
    * @param sp2   the position of the last character in the case declaration.
    */
  case class RestrictableCase(sp1: SourcePosition, ident: Name.Ident, tpe: Option[ParsedAst.Type], sp2: SourcePosition)

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
    * An equality constraint.
    *
    * @param sp1  the source position of the first character in the equality constraint.
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    * @param sp2  the source position of the last character in the equality constraint.
    */
  case class EqualityConstraint(sp1: SourcePosition, tpe1: Type, tpe2: Type, sp2: SourcePosition)

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
    * A common super-type for predicate parameters.
    */
  sealed trait PredicateParam

  object PredicateParam {

    /**
      * Represents an untyped (un-annotated) predicate parameter.
      *
      * @param sp1   the position of the first character in the predicate parameter.
      * @param ident the name of the predicate.
      * @param sp2   the position of the first character in the predicate parameter.
      */
    case class UntypedPredicateParam(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends PredicateParam

    /**
      * Represents a type-annotated relational predicate parameter.
      *
      * @param sp1   the position of the first character in the predicate parameter.
      * @param ident the name of the predicate.
      * @param tpes  the term types.
      * @param sp2   the position of the first character in the predicate parameter.
      */
    case class RelPredicateParam(sp1: SourcePosition, ident: Name.Ident, tpes: Seq[ParsedAst.Type], sp2: SourcePosition) extends PredicateParam

    /**
      * Represents a type-annotated latticenal predicate parameter.
      *
      * @param sp1   the position of the first character in the predicate parameter.
      * @param ident the name of the predicate.
      * @param tpes  the key types.
      * @param tpe   the lattice type.
      * @param sp2   the position of the first character in the predicate parameter.
      */
    case class LatPredicateParam(sp1: SourcePosition, ident: Name.Ident, tpes: Seq[ParsedAst.Type], tpe: ParsedAst.Type, sp2: SourcePosition) extends PredicateParam

  }

  /**
    * A catch rule consists of an identifier, a Java name, and a body expression.
    *
    * @param ident the identifier.
    * @param fqn   the fully-qualified Java name.
    * @param exp   the body expression.
    */
  case class CatchRule(ident: Name.Ident, fqn: Name.JavaName, exp: ParsedAst.Expression)

  /**
    * Effect handler rule.
    *
    * @param op      the operation name.
    * @param fparams the operation parameters.
    * @param exp     the body expression.
    */
  case class HandlerRule(op: Name.Ident, fparams: Seq[FormalParam], exp: ParsedAst.Expression)

  /**
    * A relational choice pattern match rule.
    *
    * @param sp1 the position of the first character in the rule.
    * @param pat the pattern of the rule.
    * @param exp the body expression of the rule.
    * @param sp2 the position of the first character in the rule.
    */
  case class RelationalChooseRule(sp1: SourcePosition, pat: Seq[ParsedAst.RelationalChoosePattern], exp: ParsedAst.Expression, sp2: SourcePosition)

  /**
    * A type match rule consists of a variable, a type, and a body expression.
    *
    * @param ident the variable of the rule.
    * @param tpe   the type of the rule
    * @param exp   the body expression of the rule.
    */
  case class TypeMatchRule(ident: Name.Ident, tpe: ParsedAst.Type, exp: ParsedAst.Expression)

  /**
    * A pattern match rule consists of a pattern, an optional pattern guard, and a body expression.
    *
    * @param pat  the pattern of the rule.
    * @param exp1 the optional guard of the rule.
    * @param exp2 the body expression of the rule.
    */
  case class MatchRule(pat: ParsedAst.Pattern, exp1: Option[ParsedAst.Expression], exp2: ParsedAst.Expression)

  /**
    * A select channel rule consists of an identifier, a channel expression, and a body expression.
    *
    * @param ident the bound identifier.
    * @param exp1  the channel expression of the rule.
    * @param exp2  the body expression of the rule.
    */
  case class SelectChannelRule(ident: Name.Ident, exp1: ParsedAst.Expression, exp2: ParsedAst.Expression)

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
    * @param sp2   the position of the last character in the annotation.
    */
  case class Annotation(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition)

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

    /**
      * Debug part of a string interpolation.
      *
      * @param sp1 the position of the first character in the expression.
      * @param exp the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class DebugPart(sp1: SourcePosition, exp: Option[ParsedAst.Expression], sp2: SourcePosition) extends InterpolationPart

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
      * @param eff   the effect of the constructor.
      * @param ident the name given to the imported constructor.
      */
    case class Constructor(fqn: Name.JavaName, sig: Seq[ParsedAst.Type], tpe: Type, eff: Option[ParsedAst.Type], ident: Name.Ident) extends JvmOp

    /**
      * Method Invocation.
      *
      * @param fqn   the fully-qualified name of the method.
      * @param sig   the types of the formal parameters.
      * @param tpe   the return type of the imported method.
      * @param eff   the effect of the imported method.
      * @param ident the optional name given to the imported method.
      */
    case class Method(fqn: Name.JavaName, sig: Seq[ParsedAst.Type], tpe: Type, eff: Option[ParsedAst.Type], ident: Option[Name.Ident]) extends JvmOp

    /**
      * Static Method Invocation.
      *
      * @param fqn   the fully-qualified name of the static method.
      * @param sig   the declared types of the formal parameters.
      * @param tpe   the return type of the imported method.
      * @param eff   the effect of the imported method.
      * @param ident the optional name given to the imported method.
      */
    case class StaticMethod(fqn: Name.JavaName, sig: Seq[ParsedAst.Type], tpe: Type, eff: Option[ParsedAst.Type], ident: Option[Name.Ident]) extends JvmOp

    /**
      * Get Object Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param tpe   the return type of the generated function.
      * @param eff   the effect of the generated function.
      * @param ident the name given to the imported field.
      */
    case class GetField(fqn: Name.JavaName, tpe: Type, eff: Option[ParsedAst.Type], ident: Name.Ident) extends JvmOp

    /**
      * Put ObjectField.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param tpe   the return type of the generated function.
      * @param eff   the effect of the generated function.
      * @param ident the name given to the imported field.
      */
    case class PutField(fqn: Name.JavaName, tpe: Type, eff: Option[ParsedAst.Type], ident: Name.Ident) extends JvmOp

    /**
      * Get Static Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param tpe   the return type of the generated function.
      * @param eff   the effect of the generated function.
      * @param ident the name given to the imported field.
      */
    case class GetStaticField(fqn: Name.JavaName, tpe: Type, eff: Option[ParsedAst.Type], ident: Name.Ident) extends JvmOp

    /**
      * Put Static Field.
      *
      * @param fqn   the fully-qualified name of the field.
      * @param tpe   the return type of the generated function.
      * @param eff   the effect of the generated function.
      * @param ident the name given to the imported field.
      */
    case class PutStaticField(fqn: Name.JavaName, tpe: Type, eff: Option[ParsedAst.Type], ident: Name.Ident) extends JvmOp

  }

  /**
    * JvmMethod (used within NewObject)
    *
    * @param sp1     the position of the first character in the method.
    * @param ident   the name of the method.
    * @param fparams the formal parameters.
    * @param exp     the method body.
    * @param tpe     the method return type.
    * @param sp2     the position of the last character in the method.
    */
  case class JvmMethod(sp1: SourcePosition, ident: Name.Ident, fparams: Seq[ParsedAst.FormalParam], tpe: ParsedAst.Type, eff: Option[ParsedAst.Type], exp: ParsedAst.Expression, sp2: SourcePosition)

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
    * @param exp   the value of the field.
    * @param sp2   the position of the last character in the field.
    */
  case class RecordField(sp1: SourcePosition, field: Name.Ident, exp: ParsedAst.Expression, sp2: SourcePosition)

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

  /**
    * A common super-type for debug kinds.
    */
  sealed trait DebugKind

  object DebugKind {
    /**
      * Print the debugged value (and nothing else).
      */
    case object Debug extends DebugKind

    /**
      * Print the debugged value prefixed with the file name and line number.
      */
    case object DebugWithLoc extends DebugKind

    /**
      * Print the debugged value prefix with the file name and line number, and the source code of the expression.
      */
    case object DebugWithLocAndSrc extends DebugKind
  }

  /**
    * Represents a super type for foreach expression fragments.
    */
  sealed trait ForFragment

  object ForFragment {

    /**
      * A generator fragment, i.e. `pattern <- xs`.
      *
      * @param sp1 the position of the first character in the fragment.
      * @param pat the pattern on the left hand side.
      * @param exp the iterable expression.
      * @param sp2 the position of the last character in the fragment.
      */
    case class Generator(sp1: SourcePosition, pat: ParsedAst.Pattern, exp: ParsedAst.Expression, sp2: SourcePosition) extends ForFragment

    /**
      * A guard fragment, i.e. `if x > 1`.
      *
      * @param sp1 the position of the first character in the fragment.
      * @param exp the guard expression.
      * @param sp2 the position of the last character in the fragment.
      */
    case class Guard(sp1: SourcePosition, exp: ParsedAst.Expression, sp2: SourcePosition) extends ForFragment

  }

  /**
    * A ParYield fragment, i.e. `pattern <- exp`.
    *
    * @param sp1 the position of the first character in the fragment.
    * @param pat the pattern.
    * @param exp the expression.
    * @param sp2 the position of the last character in the fragment.
    */
  case class ParYieldFragment(sp1: SourcePosition, pat: ParsedAst.Pattern, exp: Expression, sp2: SourcePosition)

  /**
    * The derivations of an enum. If length of `classes` is greater than zero, this represents:
    * {{{
    *   enum Abc with X, Y, Z {
    *            ^^^^^^^^^^^^
    * }}}
    *
    * If classes is empty, this represents the place where they would be inserted:
    * {{{
    *   enum Abc {
    *           ^
    * }}}
    *
    * @param sp1  the position of the first character of the derivations.
    * @param classes the derived classes.
    * @param sp2  the position of the last character of the derivations.
    */
  case class Derivations(sp1: SourcePosition, classes: Seq[Name.QName], sp2: SourcePosition)
}
