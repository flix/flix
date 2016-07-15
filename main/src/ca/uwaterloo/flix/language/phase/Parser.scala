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

import ca.uwaterloo.flix.language.ast.{Type => PType}
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
  val DefaultCharset = Charset.forName("UTF-8")

  /*
   * Initialize parser input.
   */
  override val input: ParserInput = source match {
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
      zeroOrMore(Declaration).separatedBy(optWS)
    }

    rule {
      optWS ~ Imports ~ optWS ~ Decls ~ optWS ~ EOI ~> ParsedAst.Root
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
      SP ~ atomic("import") ~ WS ~ NName ~ "/" ~ "_" ~ optSC ~ SP ~> ParsedAst.Import.Wild
    }

    def Definition: Rule1[ParsedAst.Import.Definition] = rule {
      SP ~ atomic("import") ~ WS ~ NName ~ "/" ~ Ident ~ optSC ~ SP ~> ParsedAst.Import.Definition
    }

    def Namespace: Rule1[ParsedAst.Import.Namespace] = rule {
      SP ~ atomic("import") ~ WS ~ NName ~ optSC ~ SP ~> ParsedAst.Import.Namespace
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
      Declarations.Function |
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
      SP ~ atomic("namespace") ~ WS ~ NName ~ optWS ~ '{' ~ optWS ~ zeroOrMore(Declaration).separatedBy(optWS) ~ optWS ~ '}' ~ SP ~ optSC ~> ParsedAst.Declaration.Namespace
    }

    def Function: Rule1[ParsedAst.Declaration.Definition] = {
      def Annotations: Rule1[Seq[ParsedAst.Annotation]] = rule {
        zeroOrMore(Annotation).separatedBy(WS)
      }

      rule {
        Annotations ~ optWS ~ SP ~ atomic("def") ~ WS ~ Ident ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~ optSC ~> ParsedAst.Declaration.Definition
      }
    }

    def Signature: Rule1[ParsedAst.Declaration.Signature] = rule {
      SP ~ atomic("def") ~ WS ~ Ident ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~ optSC ~> ParsedAst.Declaration.Signature
    }

    def External: Rule1[ParsedAst.Declaration.External] = rule {
      SP ~ atomic("external") ~ optWS ~ atomic("def") ~ WS ~ Ident ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~ optSC ~> ParsedAst.Declaration.External
    }

    def Law: Rule1[ParsedAst.Declaration.Law] = rule {
      SP ~ atomic("law") ~ WS ~ Ident ~ optWS ~ TypeParams ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~ optSC ~> ParsedAst.Declaration.Law
    }

    def Enum: Rule1[ParsedAst.Declaration.Enum] = {
      def UnitCase: Rule1[ParsedAst.Case] = rule {
        SP ~ atomic("case") ~ WS ~ Ident ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) => ParsedAst.Case(sp1, ident, PType.Unit, sp2))
      }

      def NestedCase: Rule1[ParsedAst.Case] = rule {
        SP ~ atomic("case") ~ WS ~ Ident ~ Type ~ SP ~> ParsedAst.Case
      }

      def Cases: Rule1[Seq[ParsedAst.Case]] = rule {
        // NB: NestedCase must be parsed before UnitCase.
        oneOrMore(NestedCase | UnitCase).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        SP ~ atomic("enum") ~ WS ~ Ident ~ optWS ~ "{" ~ optWS ~ Cases ~ optWS ~ "}" ~ SP ~ optSC ~> ParsedAst.Declaration.Enum
      }
    }

    def Class: Rule1[ParsedAst.Declaration.Class] = {

      def TypeParams: Rule1[Seq[Type]] = rule {
        "[" ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ "]"
      }

      def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
        SP ~ Ident ~ TypeParams ~ SP ~> ParsedAst.ContextBound
      }

      def ContextBounds: Rule1[Seq[ParsedAst.ContextBound]] = rule {
        optional(optWS ~ atomic("=>") ~ optWS ~ oneOrMore(ContextBound).separatedBy(optWS ~ "," ~ optWS) ~ optWS) ~>
          ((o: Option[Seq[ParsedAst.ContextBound]]) => o match {
            case None => Seq.empty
            case Some(xs) => xs
          })
      }

      def ClassBody: Rule1[Seq[ParsedAst.Declaration]] = rule {
        "{" ~ optWS ~ zeroOrMore(Function | Signature | Law).separatedBy(WS) ~ optWS ~ "}"
      }

      rule {
        SP ~ atomic("class") ~ WS ~ Ident ~ TypeParams ~ optWS ~ ContextBounds ~ optWS ~ ClassBody ~ SP ~> ParsedAst.Declaration.Class
      }
    }

    def Impl: Rule1[ParsedAst.Declaration.Impl] = {

      def TypeParams: Rule1[Seq[Type]] = rule {
        "[" ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ "]"
      }

      def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
        SP ~ Ident ~ TypeParams ~ SP ~> ParsedAst.ContextBound
      }

      def ContextBounds: Rule1[Seq[ParsedAst.ContextBound]] = rule {
        optional(optWS ~ atomic("<=") ~ optWS ~ oneOrMore(ContextBound).separatedBy(optWS ~ "," ~ optWS) ~ optWS) ~>
          ((o: Option[Seq[ParsedAst.ContextBound]]) => o match {
            case None => Seq.empty
            case Some(xs) => xs
          })
      }

      def ImplBody: Rule1[Seq[ParsedAst.Declaration.Definition]] = rule {
        "{" ~ optWS ~ zeroOrMore(Function).separatedBy(WS) ~ optWS ~ "}"
      }

      rule {
        SP ~ atomic("impl") ~ WS ~ Ident ~ TypeParams ~ optWS ~ ContextBounds ~ optWS ~ ImplBody ~ SP ~> ParsedAst.Declaration.Impl
      }
    }

    def Relation: Rule1[ParsedAst.Declaration.Relation] = rule {
      SP ~ atomic("rel") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Declaration.Relation
    }

    def Lattice: Rule1[ParsedAst.Declaration.Lattice] = rule {
      SP ~ atomic("lat") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Declaration.Lattice
    }

    def Index: Rule1[ParsedAst.Declaration.Index] = {
      def Indexes: Rule1[Seq[Name.Ident]] = rule {
        "{" ~ optWS ~ zeroOrMore(Ident).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}"
      }

      rule {
        SP ~ atomic("index") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Indexes).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Declaration.Index
      }
    }

    // TODO: It would be faster to parse Facts and Rules together.
    def Fact: Rule1[ParsedAst.Declaration.Fact] = rule {
      SP ~ Predicate ~ optWS ~ "." ~ SP ~> ParsedAst.Declaration.Fact
    }

    def Rule: Rule1[ParsedAst.Declaration.Rule] = rule {
      SP ~ Predicate ~ optWS ~ ":-" ~ optWS ~ oneOrMore(Predicate).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "." ~ SP ~> ParsedAst.Declaration.Rule
    }

    def LetLattice: Rule1[ParsedAst.Declaration] = {
      def Elms: Rule1[Seq[ParsedAst.Expression]] = rule {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        SP ~ atomic("let") ~ optWS ~ Type ~ atomic("<>") ~ optWS ~ "=" ~ optWS ~ "(" ~ optWS ~ Elms ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Declaration.BoundedLattice
      }
    }

    def TypeParams: Rule1[Seq[ParsedAst.ContextBound]] = {
      def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
        SP ~ Ident ~ optional(optWS ~ ":" ~ optWS ~ Type) ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, bound: Option[PType], sp2: SourcePosition) => bound match {
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

  def Attribute: Rule1[Ast.Attribute] = rule {
    Ident ~ optWS ~ ":" ~ optWS ~ Type ~> Ast.Attribute
  }

  def Attributes: Rule1[Seq[Ast.Attribute]] = rule {
    zeroOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS)
  }

  def FormalParams: Rule1[Option[Seq[Ast.FormalParam]]] = rule {
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
    Expressions.Logical
  }

  object Expressions {

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
      Extended ~ optional(optWS ~ "`" ~ QName ~ "`" ~ optWS ~ Extended ~ SP ~> ParsedAst.Expression.Infix)
    }

    def Extended: Rule1[ParsedAst.Expression] = rule {
      Unary ~ optional(optWS ~ Operators.ExtBinaryOpt ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.ExtendedBinary)
    }

    def Unary: Rule1[ParsedAst.Expression] = rule {
      !Literal ~ (SP ~ Operators.UnaryOp ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Unary) | Ascribe
    }

    def Ascribe: Rule1[ParsedAst.Expression] = rule {
      FList ~ optional(optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Expression.Ascribe)
    }

    def Primary: Rule1[ParsedAst.Expression] = rule {
      LetMatch | IfThenElse | Match | Switch | Tag | Lambda | Tuple | FNil | FNone | FSome | FVec | FSet | FMap | Literal |
        Existential | Universal | Bot | Top | UnaryLambda | Wild | Var | UserError
    }

    def Literal: Rule1[ParsedAst.Expression.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Expression.Lit
    }

    def IfThenElse: Rule1[ParsedAst.Expression.IfThenElse] = rule {
      SP ~ atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ WS ~ atomic("else") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
    }

    def LetMatch: Rule1[ParsedAst.Expression.LetMatch] = rule {
      SP ~ atomic("let") ~ WS ~ Pattern ~ optWS ~ "=" ~ optWS ~ Expression ~ WS ~ atomic("in") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.LetMatch
    }

    def Match: Rule1[ParsedAst.Expression.Match] = {
      def Rule: Rule1[(ParsedAst.Pattern, ParsedAst.Expression)] = rule {
        atomic("case") ~ WS ~ Pattern ~ optWS ~ atomic("=>") ~ optWS ~ Expression ~ optSC ~> ((p: ParsedAst.Pattern, e: ParsedAst.Expression) => (p, e))
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
      SP ~ QName ~ "." ~ Ident ~ optional(optWS ~ Tuple) ~ SP ~> ParsedAst.Expression.Tag
    }

    def Tuple: Rule1[ParsedAst.Expression] = rule {
      SP ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Tuple
    }

    def FNone: Rule1[ParsedAst.Expression.FNone] = rule {
      SP ~ atomic("None") ~ SP ~> ParsedAst.Expression.FNone
    }

    def FSome: Rule1[ParsedAst.Expression.FSome] = rule {
      SP ~ atomic("Some") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.FSome
    }

    def FNil: Rule1[ParsedAst.Expression.FNil] = rule {
      SP ~ atomic("Nil") ~ SP ~> ParsedAst.Expression.FNil
    }

    def FList: Rule1[ParsedAst.Expression] = rule {
      Apply ~ optional(optWS ~ atomic("::") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.FList)
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

    def Bot: Rule1[ParsedAst.Expression.Bot] = rule {
      SP ~ "⊥" ~ SP ~> ParsedAst.Expression.Bot
    }

    def Top: Rule1[ParsedAst.Expression.Top] = rule {
      SP ~ "⊤" ~ SP ~> ParsedAst.Expression.Top
    }

    def Wild: Rule1[ParsedAst.Expression.Wild] = rule {
      SP ~ atomic("_") ~ SP ~> ParsedAst.Expression.Wild
    }

    def Var: Rule1[ParsedAst.Expression.Var] = rule {
      SP ~ QName ~ SP ~> ParsedAst.Expression.Var
    }

    def UnaryLambda: Rule1[ParsedAst.Expression.Lambda] = rule {
      SP ~ Ident ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ((sp1: SourcePosition, arg: Name.Ident, body: ParsedAst.Expression, sp2: SourcePosition) =>
        ParsedAst.Expression.Lambda(sp1, Seq(arg), body, sp2))
    }

    def Lambda: Rule1[ParsedAst.Expression.Lambda] = rule {
      SP ~ "(" ~ optWS ~ oneOrMore(Ident).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Lambda
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
      FNil | FNone | FSome | Tag | Literal | Tuple | FVec | FSet | FMap | Wildcard | Variable
    }

    def Wildcard: Rule1[ParsedAst.Pattern.Wild] = rule {
      SP ~ atomic("_") ~ SP ~> ParsedAst.Pattern.Wild
    }

    def Variable: Rule1[ParsedAst.Pattern.Var] = rule {
      SP ~ Ident ~ SP ~> ParsedAst.Pattern.Var
    }

    def Literal: Rule1[ParsedAst.Pattern.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Pattern.Lit
    }

    def Tag: Rule1[ParsedAst.Pattern.Tag] = rule {
      SP ~ QName ~ "." ~ Ident ~ optional(optWS ~ Pattern) ~ SP ~> ParsedAst.Pattern.Tag
    }

    def Tuple: Rule1[ParsedAst.Pattern.Tuple] = rule {
      SP ~ "(" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Pattern.Tuple
    }

    def FNone: Rule1[ParsedAst.Pattern.FNone] = rule {
      SP ~ atomic("None") ~ SP ~> ParsedAst.Pattern.FNone
    }

    def FSome: Rule1[ParsedAst.Pattern.FSome] = rule {
      SP ~ atomic("Some") ~ optWS ~ "(" ~ optWS ~ Pattern ~ optWS ~ ")" ~ SP ~> ParsedAst.Pattern.FSome
    }

    def FNil: Rule1[ParsedAst.Pattern.FNil] = rule {
      SP ~ atomic("Nil") ~ SP ~> ParsedAst.Pattern.FNil
    }

    def FList: Rule1[ParsedAst.Pattern] = rule {
      Simple ~ optional(optWS ~ atomic("::") ~ optWS ~ Pattern ~ SP ~> ParsedAst.Pattern.FList)
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
    Predicates.True | Predicates.False | Predicates.Ambiguous | Predicates.NotEqual | Predicates.Equal | Predicates.Loop
  }

  object Predicates {
    def True: Rule1[ParsedAst.Predicate.True] = rule {
      SP ~ atomic("true") ~ SP ~> ParsedAst.Predicate.True
    }

    def False: Rule1[ParsedAst.Predicate.False] = rule {
      SP ~ atomic("false") ~ SP ~> ParsedAst.Predicate.False
    }

    def Ambiguous: Rule1[ParsedAst.Predicate.Ambiguous] = rule {
      SP ~ QName ~ optWS ~ "(" ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Ambiguous
    }

    def Equal: Rule1[ParsedAst.Predicate.Equal] = rule {
      SP ~ Ident ~ optWS ~ atomic(":=") ~ optWS ~ Expression ~ SP ~> ParsedAst.Predicate.Equal
    }

    def NotEqual: Rule1[ParsedAst.Predicate.NotEqual] = rule {
      SP ~ Ident ~ optWS ~ atomic("!=") ~ optWS ~ Ident ~ SP ~> ParsedAst.Predicate.NotEqual
    }

    def Loop: Rule1[ParsedAst.Predicate.Loop] = rule {
      SP ~ Ident ~ optWS ~ atomic("<-") ~ optWS ~ Expression ~ SP ~> ParsedAst.Predicate.Loop
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def Type: Rule1[PType] = rule {
    Types.UnaryLambda
  }

  object Types {

    def Primary: Rule1[PType] = rule {
      Lambda | Tuple | Parametric | Name
    }

    def Name: Rule1[PType] = rule {
      QName ~> PType.Unresolved
    }

    def Tuple: Rule1[PType] = {
      def Unit: Rule1[PType] = rule {
        atomic("()") ~ optWS ~> (() => PType.Unit)
      }

      def Singleton: Rule1[PType] = rule {
        "(" ~ optWS ~ Type ~ optWS ~ ")" ~ optWS
      }

      def Tuple: Rule1[PType] = rule {
        "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~> ((xs: Seq[PType]) => PType.Tuple(xs.toList))
      }

      rule {
        Unit | Singleton | Tuple
      }
    }

    def UnaryLambda: Rule1[PType] = rule {
      Primary ~ optional(optWS ~ atomic("->") ~ optWS ~ Type) ~> ((t: PType, o: Option[PType]) => o match {
        case None => t
        case Some(r) => PType.Lambda(List(t), r) // TODO: Maybe need to reverse order???
      })
    }

    def Lambda: Rule1[PType] = rule {
      "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~ atomic("->") ~ optWS ~ Type ~> ((xs: Seq[PType], r: PType) => PType.Lambda(xs.toList, r))
    }

    def Parametric: Rule1[PType] = rule {
      QName ~ optWS ~ "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ optWS ~> PType.Parametric
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Helpers                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  def ArgumentList: Rule1[Seq[Ast.FormalParam]] = rule {
    zeroOrMore(Argument).separatedBy(optWS ~ "," ~ optWS)
  }

  def Argument: Rule1[Ast.FormalParam] = rule {
    Ident ~ ":" ~ optWS ~ Type ~> Ast.FormalParam
  }

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  def LegalIdent: Rule1[String] = {
    rule {
      capture((CharPredicate.Alpha | "⊥" | "⊤" | "⊑" | "⊔" | "⊓" | "▽" | "△" | "⊡") ~ zeroOrMore(CharPredicate.AlphaNum | "_" | "$" | "⊥" | "⊑"))
    }
  }

  def Ident: Rule1[Name.Ident] = rule {
    SP ~ LegalIdent ~ SP ~> Name.Ident
  }

  def NName: Rule1[Name.NName] = rule {
    SP ~ oneOrMore(Ident).separatedBy(".") ~ SP ~>
      ((sp1: SourcePosition, parts: Seq[Name.Ident], sp2: SourcePosition) => Name.NName(sp1, parts.toList, sp2))
  }

  def QName: Rule1[Name.QName] = rule {
    SP ~ optional(NName ~ "/") ~ Ident ~ SP ~>
      ((sp1: SourcePosition, nsOpt: Option[Name.NName], ident: Name.Ident, sp2: SourcePosition) => nsOpt match {
        case None => Name.QName(sp1, Name.NName(sp1, List.empty, sp2), ident, sp2)
        case Some(ns) => Name.QName(sp1, ns, ident, sp2)
      })
  }

  def Annotation: Rule1[ParsedAst.Annotation] = rule {
    SP ~ atomic("@") ~ LegalIdent ~ SP ~> ParsedAst.Annotation
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

  def optSC: Rule0 = rule {
    optional(optWS ~ ";")
  }

  def NewLine: Rule0 = rule {
    "\n" | "\r"
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Comment: Rule0 = rule {
    Comments.SingleLineComment | Comments.MultiLineComment
  }

  object Comments {
    // Note: We must use ANY to match (consume) whatever character which is not a newline.
    // Otherwise the parser makes no progress and loops.
    def SingleLineComment: Rule0 = rule {
      "//" ~ zeroOrMore(!NewLine ~ ANY) ~ (NewLine | EOI)
    }

    // Note: We must use ANY to match (consume) whatever character which is not a "*/".
    // Otherwise the parser makes no progress and loops.
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
      push(SourcePosition(source, lineNumber, columnNumber, () => input.getLine(lineNumber)))
    }
  }

}
