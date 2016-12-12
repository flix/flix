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

package ca.uwaterloo.flix.language.phase

import java.nio.charset.Charset
import java.nio.file.Files
import java.util.zip.ZipFile

import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import ca.uwaterloo.flix.util.StreamOps
import org.parboiled2._

import scala.collection.immutable.Seq
import scala.io.Source

/**
  * A parser for the Flix language.
  */
class Parser(val source: SourceInput) extends org.parboiled2.Parser {

  /*
    * Implicitly assumed default charset.
    */
  val DefaultCharset: Charset = Charset.forName("UTF-8")

  /*
   * Initialize parser input.
   */
  override val input: ParserInput = source match {
    case SourceInput.Internal(name, text) => text
    case SourceInput.Str(str) => str
    case SourceInput.TxtFile(path) =>
      new String(Files.readAllBytes(path), DefaultCharset)
    case SourceInput.ZipFile(path) =>
      val file = new ZipFile(path.toFile)
      val entry = file.entries().nextElement()
      val inputStream = file.getInputStream(entry)
      new String(StreamOps.readAllBytes(inputStream), DefaultCharset)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Root                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  def Root: Rule1[ParsedAst.Root] = {
    def Imports: Rule1[Seq[ParsedAst.Import]] = rule {
      zeroOrMore(Import).separatedBy(optWS)
    }

    def Decls: Rule1[Seq[ParsedAst.Declaration]] = rule {
      zeroOrMore(Declaration)
    }

    rule {
      Imports ~ Decls ~ optWS ~ EOI ~> ParsedAst.Root
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Imports                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  def Import: Rule1[ParsedAst.Import] = rule {
    Imports.Wildcard | Imports.Definition | Imports.Namespace
  }

  object Imports {

    def Wildcard: Rule1[ParsedAst.Import.Wild] = rule {
      SP ~ atomic("import") ~ WS ~ Names.Namespace ~ "/" ~ "_" ~ SP ~> ParsedAst.Import.Wild
    }

    def Definition: Rule1[ParsedAst.Import.Definition] = rule {
      SP ~ atomic("import") ~ WS ~ Names.Namespace ~ "/" ~ Names.Definition ~ SP ~> ParsedAst.Import.Definition
    }

    def Namespace: Rule1[ParsedAst.Import.Namespace] = rule {
      SP ~ atomic("import") ~ WS ~ Names.Namespace ~ SP ~> ParsedAst.Import.Namespace
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations                                                            //
  /////////////////////////////////////////////////////////////////////////////
  // NB: RuleDeclaration must be parsed before FactDeclaration.
  def Declaration: Rule1[ParsedAst.Declaration] = rule {
    Declarations.Namespace |
      Declarations.Rule |
      Declarations.Fact |
      Declarations.Definition |
      Declarations.External |
      Declarations.Enum |
      Declarations.LetLattice |
      Declarations.Relation |
      Declarations.Lattice |
      Declarations.Index |
      Declarations.Law |
      Declarations.Class |
      Declarations.Impl
  }

  object Declarations {

    def Namespace: Rule1[ParsedAst.Declaration.Namespace] = rule {
      optWS ~ SP ~ atomic("namespace") ~ WS ~ Names.Namespace ~ optWS ~ '{' ~ zeroOrMore(Declaration) ~ optWS ~ '}' ~ SP ~> ParsedAst.Declaration.Namespace
    }

    def Definition: Rule1[ParsedAst.Declaration.Definition] = {
      def Annotations: Rule1[Seq[ParsedAst.Annotation]] = rule {
        zeroOrMore(Annotation).separatedBy(WS)
      }

      rule {
        Documentation ~ Annotations ~ optWS ~ SP ~ atomic("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Declaration.Definition
      }
    }

    def Signature: Rule1[ParsedAst.Declaration.Signature] = rule {
      Documentation ~ SP ~ atomic("def") ~ WS ~ Names.Definition ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Declaration.Signature
    }

    def External: Rule1[ParsedAst.Declaration.External] = rule {
      Documentation ~ SP ~ atomic("external") ~ optWS ~ atomic("def") ~ WS ~ Names.Definition ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Declaration.External
    }

    def Law: Rule1[ParsedAst.Declaration.Law] = rule {
      Documentation ~ SP ~ atomic("law") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Declaration.Law
    }

    def Enum: Rule1[ParsedAst.Declaration.Enum] = {
      def UnitCase: Rule1[ParsedAst.Case] = rule {
        SP ~ atomic("case") ~ WS ~ Names.Tag ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) =>
          ParsedAst.Case(sp1, ident, ParsedAst.Type.Unit(sp1, sp2), sp2))
      }

      def NestedCase: Rule1[ParsedAst.Case] = rule {
        SP ~ atomic("case") ~ WS ~ Names.Tag ~ Type ~ SP ~> ParsedAst.Case
      }

      def Cases: Rule1[Seq[ParsedAst.Case]] = rule {
        // NB: NestedCase must be parsed before UnitCase.
        oneOrMore(NestedCase | UnitCase).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        Documentation ~ SP ~ atomic("enum") ~ WS ~ Names.Type ~ TypeParams ~ optWS ~ "{" ~ optWS ~ Cases ~ optWS ~ "}" ~ SP ~> ParsedAst.Declaration.Enum
      }
    }

    def Class: Rule1[ParsedAst.Declaration.Class] = {

      def TypeParams: Rule1[Seq[ParsedAst.Type]] = rule {
        "[" ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ "]"
      }

      def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
        SP ~ Names.Class ~ TypeParams ~ SP ~> ParsedAst.ContextBound
      }

      def ContextBounds: Rule1[Seq[ParsedAst.ContextBound]] = rule {
        optional(optWS ~ atomic("=>") ~ optWS ~ oneOrMore(ContextBound).separatedBy(optWS ~ "," ~ optWS) ~ optWS) ~>
          ((o: Option[Seq[ParsedAst.ContextBound]]) => o match {
            case None => Seq.empty
            case Some(xs) => xs
          })
      }

      def ClassBody: Rule1[Seq[ParsedAst.Declaration]] = rule {
        "{" ~ optWS ~ zeroOrMore(Definition | Signature | Law).separatedBy(WS) ~ optWS ~ "}"
      }

      rule {
        Documentation ~ SP ~ atomic("class") ~ WS ~ Names.Class ~ TypeParams ~ optWS ~ ContextBounds ~ optWS ~ ClassBody ~ SP ~> ParsedAst.Declaration.Class
      }
    }

    def Impl: Rule1[ParsedAst.Declaration.Impl] = {

      def TypeParams: Rule1[Seq[ParsedAst.Type]] = rule {
        "[" ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ "]"
      }

      def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
        SP ~ Names.Class ~ TypeParams ~ SP ~> ParsedAst.ContextBound
      }

      def ContextBounds: Rule1[Seq[ParsedAst.ContextBound]] = rule {
        optional(optWS ~ atomic("<=") ~ optWS ~ oneOrMore(ContextBound).separatedBy(optWS ~ "," ~ optWS) ~ optWS) ~>
          ((o: Option[Seq[ParsedAst.ContextBound]]) => o match {
            case None => Seq.empty
            case Some(xs) => xs
          })
      }

      def ImplBody: Rule1[Seq[ParsedAst.Declaration.Definition]] = rule {
        "{" ~ optWS ~ zeroOrMore(Definition).separatedBy(WS) ~ optWS ~ "}"
      }

      rule {
        Documentation ~ SP ~ atomic("impl") ~ WS ~ Names.Class ~ TypeParams ~ optWS ~ ContextBounds ~ optWS ~ ImplBody ~ SP ~> ParsedAst.Declaration.Impl
      }
    }

    def Relation: Rule1[ParsedAst.Declaration.Relation] = rule {
      Documentation ~ SP ~ atomic("rel") ~ WS ~ Names.Table ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ SP ~> ParsedAst.Declaration.Relation
    }

    def Lattice: Rule1[ParsedAst.Declaration.Lattice] = rule {
      Documentation ~ SP ~ atomic("lat") ~ WS ~ Names.Table ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ SP ~> ParsedAst.Declaration.Lattice
    }

    def Index: Rule1[ParsedAst.Declaration.Index] = {
      def Indexes: Rule1[Seq[Name.Ident]] = rule {
        "{" ~ optWS ~ zeroOrMore(Names.Attribute).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}"
      }

      rule {
        optWS ~ SP ~ atomic("index") ~ WS ~ Names.QualifiedTable ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Indexes).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Declaration.Index
      }
    }

    // TODO: It would be faster to parse Facts and Rules together.
    def Fact: Rule1[ParsedAst.Declaration.Fact] = rule {
      optWS ~ SP ~ Predicate ~ optWS ~ "." ~ SP ~> ParsedAst.Declaration.Fact
    }

    def Rule: Rule1[ParsedAst.Declaration.Rule] = rule {
      optWS ~ SP ~ Predicate ~ optWS ~ ":-" ~ optWS ~ oneOrMore(Predicate).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "." ~ SP ~> ParsedAst.Declaration.Rule
    }

    def LetLattice: Rule1[ParsedAst.Declaration] = {
      def Elms: Rule1[Seq[ParsedAst.Expression]] = rule {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        optWS ~ SP ~ atomic("let") ~ optWS ~ Type ~ atomic("<>") ~ optWS ~ "=" ~ optWS ~ "(" ~ optWS ~ Elms ~ optWS ~ ")" ~ SP ~> ParsedAst.Declaration.BoundedLattice
      }
    }

    def TypeParams: Rule1[Seq[ParsedAst.ContextBound]] = {
      def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
        SP ~ Names.Variable ~ optional(optWS ~ ":" ~ optWS ~ Type) ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, bound: Option[ParsedAst.Type], sp2: SourcePosition) => bound match {
          case None => ParsedAst.ContextBound(sp1, ident, Seq.empty, sp2)
          case Some(tpe) => ParsedAst.ContextBound(sp1, ident, Seq(tpe), sp2)
        })
      }

      rule {
        optional("[" ~ optWS ~ oneOrMore(ContextBound).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]") ~> ((o: Option[Seq[ParsedAst.ContextBound]]) => o match {
          case None => Seq.empty
          case Some(xs) => xs
        })
      }
    }

  }

  def Attribute: Rule1[ParsedAst.Attribute] = rule {
    SP ~ Names.Attribute ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Attribute
  }

  def Attributes: Rule1[Seq[ParsedAst.Attribute]] = rule {
    zeroOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS)
  }

  def FormalParams: Rule1[Option[Seq[ParsedAst.FormalParam]]] = rule {
    optional("(" ~ optWS ~ ArgumentList ~ optWS ~ ")")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    Literals.Bool | Literals.Char | Literals.Float | Literals.Int | Literals.Str
  }

  object Literals {

    def Bool: Rule1[ParsedAst.Literal] = {

      def True: Rule1[ParsedAst.Literal.True] = rule {
        SP ~ atomic("true") ~ SP ~> ParsedAst.Literal.True
      }

      def False: Rule1[ParsedAst.Literal.False] = rule {
        SP ~ atomic("false") ~ SP ~> ParsedAst.Literal.False
      }

      rule {
        True | False
      }
    }

    def Char: Rule1[ParsedAst.Literal.Char] = rule {
      SP ~ "'" ~ capture(!"'" ~ CharPredicate.All) ~ "'" ~ SP ~> ParsedAst.Literal.Char
    }

    def Float: Rule1[ParsedAst.Literal] = rule {
      Float32 | Float64 | FloatDefault
    }

    def FloatDefault: Rule1[ParsedAst.Literal.Float64] = rule {
      SP ~ Sign ~ Digits ~ "." ~ Digits ~ SP ~> ParsedAst.Literal.Float64
    }

    def Float32: Rule1[ParsedAst.Literal.Float32] = rule {
      SP ~ Sign ~ Digits ~ "." ~ Digits ~ atomic("f32") ~ SP ~> ParsedAst.Literal.Float32
    }

    def Float64: Rule1[ParsedAst.Literal.Float64] = rule {
      SP ~ Sign ~ Digits ~ "." ~ Digits ~ atomic("f64") ~ SP ~> ParsedAst.Literal.Float64
    }

    def Int: Rule1[ParsedAst.Literal] = rule {
      Int8 | Int16 | Int32 | Int64 | BigInt | IntDefault
    }

    def IntDefault: Rule1[ParsedAst.Literal.Int32] = rule {
      SP ~ Sign ~ Digits ~ SP ~> ParsedAst.Literal.Int32
    }

    def Int8: Rule1[ParsedAst.Literal.Int8] = rule {
      SP ~ Sign ~ Digits ~ atomic("i8") ~ SP ~> ParsedAst.Literal.Int8
    }

    def Int16: Rule1[ParsedAst.Literal.Int16] = rule {
      SP ~ Sign ~ Digits ~ atomic("i16") ~ SP ~> ParsedAst.Literal.Int16
    }

    def Int32: Rule1[ParsedAst.Literal.Int32] = rule {
      SP ~ Sign ~ Digits ~ atomic("i32") ~ SP ~> ParsedAst.Literal.Int32
    }

    def Int64: Rule1[ParsedAst.Literal.Int64] = rule {
      SP ~ Sign ~ Digits ~ atomic("i64") ~ SP ~> ParsedAst.Literal.Int64
    }

    def BigInt: Rule1[ParsedAst.Literal.BigInt] = rule {
      SP ~ Sign ~ Digits ~ atomic("ii") ~ SP ~> ParsedAst.Literal.BigInt
    }

    def Str: Rule1[ParsedAst.Literal.Str] = rule {
      SP ~ "\"" ~ capture(zeroOrMore(!"\"" ~ CharPredicate.All)) ~ "\"" ~ SP ~> ParsedAst.Literal.Str
    }

    def Sign: Rule1[Boolean] = rule {
      optional(capture("-")) ~> ((s: Option[String]) => s.nonEmpty)
    }

    def Digits: Rule1[String] = rule {
      capture(oneOrMore(CharPredicate.Digit))
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  def Expression: Rule1[ParsedAst.Expression] = rule {
    Expressions.Block
  }

  object Expressions {

    def Block: Rule1[ParsedAst.Expression] = rule {
      "{" ~ optWS ~ Expression ~ optWS ~ "}" ~ optWS | Logical
    }

    // TODO: Improve parsing of operator precedence.

    def Logical: Rule1[ParsedAst.Expression] = rule {
      Comparison ~ optional(optWS ~ Operators.LogicalOp ~ optWS ~ Comparison ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Comparison: Rule1[ParsedAst.Expression] = rule {
      Additive ~ optional(optWS ~ Operators.ComparisonOp ~ optWS ~ Additive ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Additive: Rule1[ParsedAst.Expression] = rule {
      Multiplicative ~ zeroOrMore(optWS ~ Operators.AdditiveOp ~ optWS ~ Multiplicative ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Multiplicative: Rule1[ParsedAst.Expression] = rule {
      Infix ~ zeroOrMore(optWS ~ Operators.MultiplicativeOp ~ optWS ~ Infix ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Infix: Rule1[ParsedAst.Expression] = rule {
      Extended ~ optional(optWS ~ "`" ~ Names.QualifiedDefinition ~ "`" ~ optWS ~ Extended ~ SP ~> ParsedAst.Expression.Infix)
    }

    def Extended: Rule1[ParsedAst.Expression] = rule {
      Unary ~ optional(optWS ~ Operators.ExtBinaryOpt ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.ExtendedBinary)
    }

    def Unary: Rule1[ParsedAst.Expression] = rule {
      !Literal ~ (SP ~ Operators.UnaryOp ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Unary) | Ascribe
    }

    def Ascribe: Rule1[ParsedAst.Expression] = rule {
      FAppend ~ optional(optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Expression.Ascribe)
    }

    def Primary: Rule1[ParsedAst.Expression] = rule {
      LetMatch | IfThenElse | Match | Switch | Lambda | Tuple | FNil | FVec | FSet | FMap | Literal |
        Existential | Universal | UnaryLambda | QName | Wild | Tag | SName | UserError
    }

    def Literal: Rule1[ParsedAst.Expression.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Expression.Lit
    }

    def IfThenElse: Rule1[ParsedAst.Expression.IfThenElse] = rule {
      SP ~ atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ WS ~ atomic("else") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
    }

    def LetMatch: Rule1[ParsedAst.Expression.LetMatch] = rule {
      SP ~ atomic("let") ~ WS ~ Pattern ~ optWS ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.LetMatch
    }

    def Match: Rule1[ParsedAst.Expression.Match] = {
      def Rule: Rule1[(ParsedAst.Pattern, ParsedAst.Expression)] = rule {
        atomic("case") ~ WS ~ Pattern ~ optWS ~ atomic("=>") ~ optWS ~ Expression ~> ((p: ParsedAst.Pattern, e: ParsedAst.Expression) => (p, e))
      }

      rule {
        SP ~ atomic("match") ~ WS ~ Expression ~ WS ~ atomic("with") ~ WS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Match
      }
    }

    def Switch: Rule1[ParsedAst.Expression.Switch] = {
      def Rule: Rule1[(ParsedAst.Expression, ParsedAst.Expression)] = rule {
        atomic("case") ~ WS ~ Expression ~ optWS ~ "=>" ~ optWS ~ Expression ~> ((e1: ParsedAst.Expression, e2: ParsedAst.Expression) => (e1, e2))
      }

      rule {
        SP ~ atomic("switch") ~ WS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Switch
      }
    }

    def Apply: Rule1[ParsedAst.Expression] = rule {
      Primary ~ optional(optWS ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Apply)
    }

    def Tag: Rule1[ParsedAst.Expression.Tag] = rule {
      SP ~ optional(Names.QualifiedType ~ ".") ~ Names.Tag ~ optional(optWS ~ Tuple) ~ SP ~> ParsedAst.Expression.Tag
    }

    def Tuple: Rule1[ParsedAst.Expression] = rule {
      SP ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Tuple
    }

    def FNil: Rule1[ParsedAst.Expression.FNil] = rule {
      SP ~ atomic("Nil") ~ SP ~> ParsedAst.Expression.FNil
    }

    def FAppend: Rule1[ParsedAst.Expression] = rule {
      FList ~ optional(optWS ~ SP ~ atomic(":::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FAppend)
    }

    def FList: Rule1[ParsedAst.Expression] = rule {
      Apply ~ optional(optWS ~ SP ~ atomic("::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FCons)
    }

    def FVec: Rule1[ParsedAst.Expression.FVec] = rule {
      SP ~ "#[" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.FVec
    }

    def FSet: Rule1[ParsedAst.Expression.FSet] = rule {
      SP ~ "#{" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.FSet
    }

    def FMap: Rule1[ParsedAst.Expression.FMap] = {
      def KeyValue: Rule1[(ParsedAst.Expression, ParsedAst.Expression)] = rule {
        Expression ~ optWS ~ atomic("->") ~ optWS ~ Expression ~> ((e1: ParsedAst.Expression, e2: ParsedAst.Expression) => (e1, e2))
      }

      rule {
        SP ~ "@{" ~ optWS ~ zeroOrMore(KeyValue).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.FMap
      }
    }

    def Wild: Rule1[ParsedAst.Expression.Wild] = rule {
      SP ~ atomic("_") ~ SP ~> ParsedAst.Expression.Wild
    }

    def SName: Rule1[ParsedAst.Expression.SName] = rule {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Expression.SName
    }

    def QName: Rule1[ParsedAst.Expression.QName] = rule {
      SP ~ Names.QualifiedDefinition ~ SP ~> ParsedAst.Expression.QName
    }

    def UnaryLambda: Rule1[ParsedAst.Expression.Lambda] = rule {
      SP ~ Names.Variable ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ((sp1: SourcePosition, arg: Name.Ident, body: ParsedAst.Expression, sp2: SourcePosition) =>
        ParsedAst.Expression.Lambda(sp1, Seq(arg), body, sp2))
    }

    def Lambda: Rule1[ParsedAst.Expression.Lambda] = rule {
      SP ~ "(" ~ optWS ~ oneOrMore(Names.Variable).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Lambda
    }

    def UserError: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("???") ~ SP ~> ParsedAst.Expression.UserError
    }

    def Existential: Rule1[ParsedAst.Expression.Existential] = rule {
      SP ~ atomic("∃" | "\\exists") ~ optWS ~ FormalParams ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Existential
    }

    def Universal: Rule1[ParsedAst.Expression.Universal] = rule {
      SP ~ atomic("∀" | "\\forall") ~ optWS ~ FormalParams ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Universal
    }

  }


  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  // NB: List must be parsed before everything.
  // NB: Literal must be parsed before Variable.
  // NB: Tag must be before Literal and Variable.
  def Pattern: Rule1[ParsedAst.Pattern] = rule {
    Patterns.FList
  }

  object Patterns {

    def Simple: Rule1[ParsedAst.Pattern] = rule {
      FNil | Tag | Literal | Tuple | FVec | FSet | FMap | Wildcard | Variable
    }

    def Wildcard: Rule1[ParsedAst.Pattern.Wild] = rule {
      SP ~ atomic("_") ~ SP ~> ParsedAst.Pattern.Wild
    }

    def Variable: Rule1[ParsedAst.Pattern.Var] = rule {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Pattern.Var
    }

    def Literal: Rule1[ParsedAst.Pattern.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Pattern.Lit
    }

    def Tag: Rule1[ParsedAst.Pattern.Tag] = rule {
      SP ~ optional(Names.QualifiedType ~ ".") ~ Names.Tag ~ optional(optWS ~ Pattern) ~ SP ~> ParsedAst.Pattern.Tag
    }

    def Tuple: Rule1[ParsedAst.Pattern.Tuple] = rule {
      SP ~ "(" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Pattern.Tuple
    }

    def FNil: Rule1[ParsedAst.Pattern.FNil] = rule {
      SP ~ atomic("Nil") ~ SP ~> ParsedAst.Pattern.FNil
    }

    def FList: Rule1[ParsedAst.Pattern] = rule {
      Simple ~ optional(optWS ~ SP ~ atomic("::") ~ SP ~ optWS ~ Pattern ~> ParsedAst.Pattern.FCons)
    }

    def FVec: Rule1[ParsedAst.Pattern.FVec] = {
      def DotDotDot: Rule1[Option[ParsedAst.Pattern]] = rule {
        optional(optWS ~ "," ~ optWS ~ Pattern ~ atomic("..."))
      }

      def Elements: Rule1[Seq[ParsedAst.Pattern]] = rule {
        zeroOrMore(!(Pattern ~ atomic("...")) ~ Pattern).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        SP ~ "#[" ~ optWS ~ Elements ~ DotDotDot ~ optWS ~ "]" ~ SP ~> ParsedAst.Pattern.FVec
      }
    }

    def FSet: Rule1[ParsedAst.Pattern.FSet] = {
      def DotDotDot: Rule1[Option[ParsedAst.Pattern]] = rule {
        optional(optWS ~ "," ~ optWS ~ Pattern ~ atomic("..."))
      }

      def Elements: Rule1[Seq[ParsedAst.Pattern]] = rule {
        zeroOrMore(!(Pattern ~ atomic("...")) ~ Pattern).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        SP ~ "#{" ~ optWS ~ Elements ~ DotDotDot ~ optWS ~ "}" ~ SP ~> ParsedAst.Pattern.FSet
      }
    }

    def FMap: Rule1[ParsedAst.Pattern.FMap] = {
      def KeyValue: Rule1[(ParsedAst.Pattern, ParsedAst.Pattern)] = rule {
        Pattern ~ optWS ~ atomic("->") ~ optWS ~ Pattern ~> ((p1: ParsedAst.Pattern, p2: ParsedAst.Pattern) => (p1, p2))
      }

      def Elements: Rule1[Seq[(ParsedAst.Pattern, ParsedAst.Pattern)]] = rule {
        zeroOrMore(KeyValue).separatedBy(optWS ~ "," ~ optWS)
      }

      def DotDotDot: Rule1[Option[ParsedAst.Pattern]] = rule {
        optional(optWS ~ "," ~ optWS ~ Pattern ~ atomic("..."))
      }

      rule {
        SP ~ "@{" ~ optWS ~ Elements ~ DotDotDot ~ optWS ~ "}" ~ SP ~> ParsedAst.Pattern.FMap
      }
    }

  }


  /////////////////////////////////////////////////////////////////////////////
  // Predicates                                                              //
  /////////////////////////////////////////////////////////////////////////////
  def Predicate: Rule1[ParsedAst.Predicate] = rule {
    Predicates.True | Predicates.False | Predicates.Filter | Predicates.Table | Predicates.NotEqual | Predicates.Loop
  }

  object Predicates {
    def True: Rule1[ParsedAst.Predicate.True] = rule {
      SP ~ atomic("true") ~ SP ~> ParsedAst.Predicate.True
    }

    def False: Rule1[ParsedAst.Predicate.False] = rule {
      SP ~ atomic("false") ~ SP ~> ParsedAst.Predicate.False
    }

    def Filter: Rule1[ParsedAst.Predicate.Filter] = rule {
      SP ~ Names.QualifiedDefinition ~ optWS ~ "(" ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Filter
    }

    def Table: Rule1[ParsedAst.Predicate.Table] = rule {
      SP ~ Names.QualifiedTable ~ optWS ~ "(" ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Table
    }

    def NotEqual: Rule1[ParsedAst.Predicate.NotEqual] = rule {
      SP ~ Names.Variable ~ optWS ~ atomic("!=") ~ optWS ~ Names.Variable ~ SP ~> ParsedAst.Predicate.NotEqual
    }

    def Loop: Rule1[ParsedAst.Predicate.Loop] = rule {
      SP ~ Names.Variable ~ optWS ~ atomic("<-") ~ optWS ~ Expression ~ SP ~> ParsedAst.Predicate.Loop
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def Type: Rule1[ParsedAst.Type] = rule {
    Types.UnaryArrow
  }

  object Types {

    def Primary: Rule1[ParsedAst.Type] = rule {
      Arrow | Tuple | Apply | Var | Ref
    }

    def Var: Rule1[ParsedAst.Type] = rule {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Type.Var
    }

    def Ref: Rule1[ParsedAst.Type] = rule {
      SP ~ Names.QualifiedType ~ SP ~> ParsedAst.Type.Ref
    }

    def Tuple: Rule1[ParsedAst.Type] = {
      def Unit: Rule1[ParsedAst.Type] = rule {
        SP ~ atomic("()") ~ SP ~ optWS ~> ParsedAst.Type.Unit
      }

      def Singleton: Rule1[ParsedAst.Type] = rule {
        "(" ~ optWS ~ Type ~ optWS ~ ")" ~ optWS
      }

      def Tuple: Rule1[ParsedAst.Type] = rule {
        SP ~ "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~ optWS ~> ParsedAst.Type.Tuple
      }

      rule {
        Unit | Singleton | Tuple
      }
    }

    def UnaryArrow: Rule1[ParsedAst.Type] = rule {
      SP ~ Primary ~ optional(optWS ~ atomic("->") ~ optWS ~ Type) ~ SP ~> ((sp1: SourcePosition, t: ParsedAst.Type, o: Option[ParsedAst.Type], sp2: SourcePosition) => o match {
        case None => t
        case Some(r) => ParsedAst.Type.Arrow(sp1, List(t), r, sp2) // TODO: Maybe need to reverse order???
      })
    }

    def Arrow: Rule1[ParsedAst.Type] = rule {
      SP ~ "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~ atomic("->") ~ optWS ~ Type ~ SP ~> ParsedAst.Type.Arrow
    }

    def Apply: Rule1[ParsedAst.Type] = rule {
      SP ~ Ref ~ optWS ~ "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~ optWS ~> ParsedAst.Type.Apply
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Helpers                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  def ArgumentList: Rule1[Seq[ParsedAst.FormalParam]] = rule {
    zeroOrMore(Argument).separatedBy(optWS ~ "," ~ optWS)
  }

  def Argument: Rule1[ParsedAst.FormalParam] = rule {
    SP ~ Names.Variable ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.FormalParam
  }

  def Annotation: Rule1[ParsedAst.Annotation] = rule {
    SP ~ atomic("@") ~ Names.Annotation ~ SP ~> ParsedAst.Annotation
  }

  /////////////////////////////////////////////////////////////////////////////
  // Names                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  object Names {

    /**
      * A lowercase letter.
      */
    def LowerLetter: CharPredicate = CharPredicate.LowerAlpha

    /**
      * An uppercase letter.
      */
    def UpperLetter: CharPredicate = CharPredicate.UpperAlpha

    /**
      * A greek letter.
      */
    def GreekLetter: CharPredicate = CharPredicate('\u0370' to '\u03FF')

    /**
      * a (upper/lower case) letter, numeral, greek letter, or other legal character.
      */
    def LegalChar: CharPredicate = CharPredicate.AlphaNum ++ GreekLetter ++ "_" ++ "'"

    /**
      * A lowercase identifier is a lowercase letter optionally followed by any letter, underscore, or prime.
      */
    def LowerCaseName: Rule1[Name.Ident] = rule {
      SP ~ capture((LowerLetter | GreekLetter) ~ zeroOrMore(LegalChar)) ~ SP ~> Name.Ident
    }

    /**
      * An uppercase identifier is an uppercase letter optionally followed by any letter, underscore, or prime.
      */
    def UpperCaseName: Rule1[Name.Ident] = rule {
      SP ~ capture(UpperLetter ~ zeroOrMore(LegalChar)) ~ SP ~> Name.Ident
    }

    /**
      * A lowercase qualified name is a namespace followed by a lowercase name.
      */
    def LowerCaseQName: Rule1[Name.QName] = rule {
      SP ~ optional(Namespace ~ "/") ~ LowerCaseName ~ SP ~> Name.QName.mk _
    }

    /**
      * An uppercase qualified name is a namespace followed by an uppercase name.
      */
    def UpperCaseQName: Rule1[Name.QName] = rule {
      SP ~ optional(Namespace ~ "/") ~ UpperCaseName ~ SP ~> Name.QName.mk _
    }

    /**
      * Namespaces are lower or uppercase.
      */
    // TODO: In the future we should restrict namespaces to either lower/upper case.
    // TODO: In Java/C++ namespaces are lower. In Haskell they are upper.
    def Namespace: Rule1[Name.NName] = rule {
      SP ~ oneOrMore(LowerCaseName | UpperCaseName).separatedBy(".") ~ SP ~>
        ((sp1: SourcePosition, parts: Seq[Name.Ident], sp2: SourcePosition) => Name.NName(sp1, parts.toList, sp2))
    }

    def Annotation: Rule1[Name.Ident] = LowerCaseName

    def Attribute: Rule1[Name.Ident] = LowerCaseName

    def Class: Rule1[Name.Ident] = UpperCaseName

    def Definition: Rule1[Name.Ident] = LowerCaseName

    def QualifiedDefinition: Rule1[Name.QName] = LowerCaseQName

    def Table: Rule1[Name.Ident] = UpperCaseName

    def QualifiedTable: Rule1[Name.QName] = UpperCaseQName

    def Tag: Rule1[Name.Ident] = UpperCaseName

    def Type: Rule1[Name.Ident] = UpperCaseName

    def QualifiedType: Rule1[Name.QName] = UpperCaseQName

    def Variable: Rule1[Name.Ident] = LowerCaseName
    
  }

  /////////////////////////////////////////////////////////////////////////////
  // Operators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  object Operators {

    /**
      * Parses a unary operator.
      */
    def UnaryOp: Rule1[UnaryOperator] = rule {
      atomic("!") ~> (() => UnaryOperator.LogicalNot) |
        atomic("+") ~> (() => UnaryOperator.Plus) |
        atomic("-") ~> (() => UnaryOperator.Minus) |
        atomic("~") ~> (() => UnaryOperator.BitwiseNegate) |
        atomic("¬") ~> (() => UnaryOperator.LogicalNot)
    }

    /**
      * Parses a logical operator.
      */
    def LogicalOp: Rule1[BinaryOperator] = rule {
      atomic("&&") ~> (() => BinaryOperator.LogicalAnd) |
        atomic("||") ~> (() => BinaryOperator.LogicalOr) |
        atomic("&") ~> (() => BinaryOperator.BitwiseAnd) |
        atomic("|") ~> (() => BinaryOperator.BitwiseOr) |
        atomic("==>") ~> (() => BinaryOperator.Implication) |
        atomic("<==>") ~> (() => BinaryOperator.Biconditional) |
        atomic("^") ~> (() => BinaryOperator.BitwiseXor) |
        atomic("<<") ~> (() => BinaryOperator.BitwiseLeftShift) |
        atomic(">>") ~> (() => BinaryOperator.BitwiseRightShift) |
        atomic("∧") ~> (() => BinaryOperator.LogicalAnd) |
        atomic("∨") ~> (() => BinaryOperator.LogicalOr) |
        atomic("→") ~> (() => BinaryOperator.Implication) |
        atomic("↔") ~> (() => BinaryOperator.Biconditional)
    }

    /**
      * Parses a comparison operator.
      */
    def ComparisonOp: Rule1[BinaryOperator] = rule {
      atomic("<=") ~> (() => BinaryOperator.LessEqual) |
        atomic(">=") ~> (() => BinaryOperator.GreaterEqual) |
        atomic("<") ~> (() => BinaryOperator.Less) |
        atomic(">") ~> (() => BinaryOperator.Greater) |
        atomic("==") ~> (() => BinaryOperator.Equal) |
        atomic("!=") ~> (() => BinaryOperator.NotEqual) |
        atomic("≡") ~> (() => BinaryOperator.Equal)
    }

    /**
      * Parses a multiplicative operator.
      */
    def MultiplicativeOp: Rule1[BinaryOperator] = rule {
      atomic("**") ~> (() => BinaryOperator.Exponentiate) |
        atomic("*") ~> (() => BinaryOperator.Times) |
        atomic("/") ~> (() => BinaryOperator.Divide) |
        atomic("%") ~> (() => BinaryOperator.Modulo)
    }

    /**
      * Parses an additive operator.
      */
    def AdditiveOp: Rule1[BinaryOperator] = rule {
      atomic("+") ~> (() => BinaryOperator.Plus) |
        atomic("-") ~> (() => BinaryOperator.Minus)
    }

    /**
      * Parses an extended binary operator.
      */
    def ExtBinaryOpt: Rule1[ExtBinaryOperator] = rule {
      atomic("⊑") ~> (() => ExtBinaryOperator.Leq) |
        atomic("⊔") ~> (() => ExtBinaryOperator.Lub) |
        atomic("⊓") ~> (() => ExtBinaryOperator.Glb) |
        atomic("▽") ~> (() => ExtBinaryOperator.Widen) |
        atomic("△") ~> (() => ExtBinaryOperator.Narrow)
    }

  }


  /////////////////////////////////////////////////////////////////////////////
  // Whitespace                                                              //
  /////////////////////////////////////////////////////////////////////////////
  def WS: Rule0 = rule {
    oneOrMore(" " | "\t" | NewLine | Comment)
  }

  def optWS: Rule0 = rule {
    optional(WS)
  }

  def NewLine: Rule0 = rule {
    "\n" | "\r"
  }

  /////////////////////////////////////////////////////////////////////////////
  // Documentation                                                           //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Optionally a parses a documentation comment.
    */
  def Documentation: Rule1[Option[ParsedAst.Documentation]] = {
    // Matches real whitespace.
    def PureWS: Rule0 = rule {
      zeroOrMore(" " | "\t" | NewLine)
    }

    // Matches triple dashed comments.
    def TripleSlash: Rule1[ParsedAst.Documentation] = rule {
      SP ~ oneOrMore(PureWS ~ "///" ~ capture(zeroOrMore(!NewLine ~ ANY)) ~ (NewLine | EOI)) ~ SP ~> ParsedAst.Documentation
    }

    // Optionally matches a triple dashed comment and then any whitespace.
    rule {
      optional(TripleSlash) ~ optWS
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Comment: Rule0 = rule {
    Comments.SingleLineComment | Comments.MultiLineComment
  }

  object Comments {
    /**
      * Parses a single line comment.
      */
    def SingleLineComment: Rule0 = rule {
      "//" ~ zeroOrMore(!NewLine ~ ANY) ~ (NewLine | EOI)
    }

    /**
      * Parses a multi line start comment.
      */
    def MultiLineComment: Rule0 = rule {
      "/*" ~ zeroOrMore(!"*/" ~ ANY) ~ "*/"
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Source Positions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  def mkLineAndColumnMaps(): (Array[Int], Array[Int]) = {
    val lines = new Array[Int](input.length + 1)
    val columns = new Array[Int](input.length + 1)

    var line = 1
    var column = 1
    for (i <- 0 until input.length) {
      lines(i) = line
      columns(i) = column
      if (input.charAt(i) == '\n') {
        line = line + 1
        column = 1
      } else {
        column = column + 1
      }
    }
    lines(input.length) = line
    columns(input.length) = column
    (lines, columns)
  }

  val (cursor2line, cursor2column) = mkLineAndColumnMaps()

  def SP: Rule1[SourcePosition] = {
    val lineNumber = cursor2line(cursor)
    val columnNumber = cursor2column(cursor)
    rule {
      push(SourcePosition(source, lineNumber, columnNumber, Some(input)))
    }
  }

}
