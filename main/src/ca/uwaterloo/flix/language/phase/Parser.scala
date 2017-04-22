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

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import ca.uwaterloo.flix.util.{StreamOps, Timer}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import org.parboiled2._

import scala.collection.immutable.Seq

object Parser {

  /**
    * Returns the AST of the given source input `source`.
    */
  def parse(source: SourceInput): Validation[ParsedAst.Root, CompilationError] = {
    val parser = new Parser(source)
    parser.Root.run() match {
      case scala.util.Success(ast) =>
        ast.toSuccess
      case scala.util.Failure(e: org.parboiled2.ParseError) =>
        ca.uwaterloo.flix.language.errors.ParseError(parser.formatError(e), source).toFailure
      case scala.util.Failure(e) =>
        ca.uwaterloo.flix.language.errors.ParseError(e.getMessage, source).toFailure
    }
  }

  /**
    * Returns the parsed AST of the given source inputs `sources`.
    */
  def parseAll(sources: List[SourceInput], hooks: Map[Symbol.DefnSym, Ast.Hook]): Validation[ParsedAst.Program, CompilationError] = {
    val timer = new Timer({
      @@(sources.map(parse)) map {
        case asts => ParsedAst.Program(asts, hooks, Time.Default)
      }
    })

    timer.getResult.map {
      case ast => ast.copy(time = ast.time.copy(parser = timer.getDuration))
    }
  }

}

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
      Declarations.Constraint |
      Declarations.Definition |
      Declarations.Enum |
      Declarations.TypeDecl |
      Declarations.LetLattice |
      Declarations.Relation |
      Declarations.Lattice |
      Declarations.Index |
      Declarations.Law
  }

  object Declarations {

    def Namespace: Rule1[ParsedAst.Declaration.Namespace] = rule {
      optWS ~ SP ~ atomic("namespace") ~ WS ~ Names.Namespace ~ optWS ~ '{' ~ zeroOrMore(Declaration) ~ optWS ~ '}' ~ SP ~> ParsedAst.Declaration.Namespace
    }

    def Definition: Rule1[ParsedAst.Declaration.Definition] = {
      def Annotations: Rule1[Seq[ParsedAst.AnnotationOrProperty]] = rule {
        zeroOrMore(Annotation | Property).separatedBy(WS)
      }

      rule {
        Documentation ~ Annotations ~ optWS ~ SP ~ atomic("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Declaration.Definition
      }
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

    def TypeDecl: Rule1[ParsedAst.Declaration.Type] = {
      def UnitCase: Rule1[ParsedAst.Case] = rule {
        SP ~ Names.Tag ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) =>
          ParsedAst.Case(sp1, ident, ParsedAst.Type.Unit(sp1, sp2), sp2))
      }

      def NestedCase: Rule1[ParsedAst.Case] = rule {
        SP ~ Names.Tag ~ Type ~ SP ~> ParsedAst.Case
      }

      rule {
        // NB: NestedCase must be parsed before UnitCase.
        Documentation ~ SP ~ atomic("type") ~ WS ~ Names.Type ~ WS ~ "=" ~ WS ~ (NestedCase | UnitCase) ~ SP ~> ParsedAst.Declaration.Type
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

    def Constraint: Rule1[ParsedAst.Declaration.Constraint] = {
      // A conjunction of head predicates: P(..) && P(..) && ...
      def HeadConj: Rule1[Seq[ParsedAst.Predicate.Head]] = rule {
        oneOrMore(HeadPredicate).separatedBy(optWS ~ "&&" ~ optWS)
      }

      // A disjunction of body predicates: P(..) || P(..) || ...
      def BodyConj: Rule1[Seq[ParsedAst.Predicate.Body]] = rule {
        oneOrMore(BodyPredicate).separatedBy(optWS ~ "||" ~ optWS)
      }

      def Body: Rule1[Seq[Seq[ParsedAst.Predicate.Body]]] = rule {
        optional(optWS ~ ":-" ~ optWS ~ oneOrMore(BodyConj).separatedBy(optWS ~ "," ~ optWS)) ~> ((o: Option[Seq[Seq[ParsedAst.Predicate.Body]]]) => o match {
          case None => Seq.empty
          case Some(xs) => xs
        })
      }

      rule {
        optWS ~ SP ~ HeadConj ~ Body ~ optWS ~ "." ~ SP ~> ParsedAst.Declaration.Constraint
      }
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
    optional("(" ~ optWS ~ FormalParamList ~ optWS ~ ")")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    Literals.Bool | Literals.Char | Literals.Str | Literals.Float | Literals.Int
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
      "{" ~ optWS ~ Expression ~ optWS ~ "}" ~ optWS | LogicalOr
    }

    def LogicalOr: Rule1[ParsedAst.Expression] = rule {
      LogicalAnd ~ zeroOrMore(optWS ~ capture(atomic("||")) ~ optWS ~ LogicalAnd ~ SP ~> ParsedAst.Expression.Binary)
    }

    def LogicalAnd: Rule1[ParsedAst.Expression] = rule {
      BitwiseOr ~ zeroOrMore(optWS ~ capture(atomic("&&")) ~ optWS ~ BitwiseOr ~ SP ~> ParsedAst.Expression.Binary)
    }

    def BitwiseOr: Rule1[ParsedAst.Expression] = rule {
      BitwiseXOr ~ zeroOrMore(optWS ~ capture(atomic("|||")) ~ optWS ~ BitwiseXOr ~ SP ~> ParsedAst.Expression.Binary)
    }

    def BitwiseXOr: Rule1[ParsedAst.Expression] = rule {
      BitwiseAnd ~ zeroOrMore(optWS ~ capture(atomic("^^^")) ~ optWS ~ BitwiseAnd ~ SP ~> ParsedAst.Expression.Binary)
    }

    def BitwiseAnd: Rule1[ParsedAst.Expression] = rule {
      Equality ~ zeroOrMore(optWS ~ capture(atomic("&&&")) ~ optWS ~ Equality ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Equality: Rule1[ParsedAst.Expression] = rule {
      Relational ~ optional(optWS ~ capture(atomic("==") | atomic("!=")) ~ optWS ~ Relational ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Relational: Rule1[ParsedAst.Expression] = rule {
      Shift ~ optional(optWS ~ capture(atomic("<=") | atomic(">=") | atomic("<") | atomic(">")) ~ optWS ~ Shift ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Shift: Rule1[ParsedAst.Expression] = rule {
      Additive ~ optional(optWS ~ capture(atomic("<<<") | atomic(">>>")) ~ optWS ~ Additive ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Additive: Rule1[ParsedAst.Expression] = rule {
      Multiplicative ~ zeroOrMore(optWS ~ capture(atomic("+") | atomic("-")) ~ optWS ~ Multiplicative ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Multiplicative: Rule1[ParsedAst.Expression] = rule {
      Infix ~ zeroOrMore(optWS ~ capture(atomic("**") | atomic("*") | atomic("/") | atomic("%")) ~ optWS ~ Infix ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Infix: Rule1[ParsedAst.Expression] = rule {
      Special ~ zeroOrMore(optWS ~ "`" ~ Names.QualifiedDefinition ~ "`" ~ optWS ~ Special ~ SP ~> ParsedAst.Expression.Infix)
    }

    def Special: Rule1[ParsedAst.Expression] = {

      // NB: We allow any operator, other than a reserved operator, to be matched by this rule.
      def Reserved2: Rule1[String] = rule {
        capture("**" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "=>" | "->")
      }

      // NB: We allow any operator, other than a reserved operator, to be matched by this rule.
      def Reserved3: Rule1[String] = rule {
        capture("<<<" | ">>>")
      }

      // Match any two character operator which is not reserved.
      def UserOp2: Rule1[String] = rule {
        !Reserved2 ~ capture(Names.OperatorLetter ~ Names.OperatorLetter)
      }

      // Match any three character operator which is not reserved.
      def UserOp3: Rule1[String] = rule {
        !Reserved3 ~ capture(Names.OperatorLetter ~ Names.OperatorLetter ~ Names.OperatorLetter)
      }

      // Match any operator which has at least three characters.
      def UserOpN: Rule1[String] = rule {
        capture(Names.OperatorLetter ~ Names.OperatorLetter ~ Names.OperatorLetter ~ oneOrMore(Names.OperatorLetter))
      }

      // Match any mathematical operator or symbol.
      def MathOp: Rule1[String] = rule {
        capture(Names.MathLetter)
      }

      rule {
        // NB: UserOpN must occur before UserOp2.
        Unary ~ zeroOrMore(optWS ~ (UserOpN | UserOp3 | UserOp2 | MathOp) ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Binary)
      }
    }

    def Unary: Rule1[ParsedAst.Expression] = rule {
      !Literal ~ (SP ~ capture(atomic("!") | atomic("+") | atomic("-") | atomic("~~~")) ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Unary) | Ascribe
    }

    def Ascribe: Rule1[ParsedAst.Expression] = rule {
      FAppend ~ optional(optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Expression.Ascribe)
    }

    def Primary: Rule1[ParsedAst.Expression] = rule {
      LetMatch | IfThenElse | Match | LambdaMatch | Switch | Unsafe | Native | Lambda | Tuple | FNil | FSet | FMap | Literal |
        Existential | Universal | UnaryLambda | QName | Wild | Tag | SName | UserError
    }

    def Literal: Rule1[ParsedAst.Expression.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Expression.Lit
    }

    def IfThenElse: Rule1[ParsedAst.Expression.IfThenElse] = rule {
      SP ~ atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ WS ~ atomic("else") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
    }

    def LetMatch: Rule1[ParsedAst.Expression.LetMatch] = rule {
      SP ~ atomic("let") ~ WS ~ Pattern ~ optWS ~ optional(":" ~ optWS ~ Type ~ optWS) ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.LetMatch
    }

    def Match: Rule1[ParsedAst.Expression.Match] = {
      def Rule: Rule1[ParsedAst.MatchRule] = rule {
        atomic("case") ~ WS ~ Pattern ~ optWS ~ optional(atomic("if") ~ WS ~ Expression ~ optWS) ~ atomic("=>") ~ optWS ~ Expression ~> ParsedAst.MatchRule
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

    def Unsafe: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("unsafe") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Unsafe
    }

    def Native: Rule1[ParsedAst.Expression] = {
      def JavaIdentifier: Rule1[String] = rule {
        capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum))
      }

      def JavaName: Rule1[Seq[String]] = rule {
        oneOrMore(JavaIdentifier).separatedBy(".")
      }

      def NativeConstructor: Rule1[ParsedAst.Expression.NativeConstructor] = rule {
        atomic("new") ~ WS ~ SP ~ JavaName ~ optWS ~ ArgumentList ~ SP ~> ParsedAst.Expression.NativeConstructor
      }

      def NativeField: Rule1[ParsedAst.Expression.NativeField] = rule {
        atomic("field") ~ WS ~ SP ~ JavaName ~ SP ~> ParsedAst.Expression.NativeField
      }

      def NativeMethod: Rule1[ParsedAst.Expression.NativeMethod] = rule {
        atomic("method") ~ WS ~ SP ~ JavaName ~ optWS ~ ArgumentList ~ SP ~> ParsedAst.Expression.NativeMethod
      }

      rule {
        atomic("native") ~ WS ~ (NativeField | NativeMethod | NativeConstructor)
      }
    }

    def Apply: Rule1[ParsedAst.Expression] = rule {
      Postfix ~ optional(ArgumentList ~ SP ~> ParsedAst.Expression.Apply)
    }

    def Postfix: Rule1[ParsedAst.Expression] = rule {
      Primary ~ zeroOrMore(optWS ~ "." ~ Names.Definition ~ ArgumentList ~ SP ~> ParsedAst.Expression.Postfix)
    }

    def Tag: Rule1[ParsedAst.Expression.Tag] = rule {
      SP ~ Names.QualifiedTag ~ optional(optWS ~ Tuple) ~ SP ~> ParsedAst.Expression.Tag
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

    def LambdaMatch: Rule1[ParsedAst.Expression.LambdaMatch] = rule {
      SP ~ atomic("match") ~ optWS ~ Pattern ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.LambdaMatch
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
      FNil | Tag | Literal | Tuple | FSet | FMap | Wildcard | Variable
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
      SP ~ Names.QualifiedTag ~ optional(optWS ~ Pattern) ~ SP ~> ParsedAst.Pattern.Tag
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
  def HeadPredicate: Rule1[ParsedAst.Predicate.Head] = rule {
    Predicates.Head.True | Predicates.Head.False | Predicates.Head.Positive | Predicates.Head.Negative
  }

  def BodyPredicate: Rule1[ParsedAst.Predicate.Body] = rule {
    Predicates.Body.Positive | Predicates.Body.Negative | Predicates.Body.Filter | Predicates.Body.NotEqual | Predicates.Body.Loop
  }

  object Predicates {

    object Head {
      def True: Rule1[ParsedAst.Predicate.Head.True] = rule {
        SP ~ atomic("true") ~ SP ~> ParsedAst.Predicate.Head.True
      }

      def False: Rule1[ParsedAst.Predicate.Head.False] = rule {
        SP ~ atomic("false") ~ SP ~> ParsedAst.Predicate.Head.False
      }

      def Positive: Rule1[ParsedAst.Predicate.Head.Positive] = rule {
        SP ~ Names.QualifiedTable ~ optWS ~ NonEmptyArgumentList ~ SP ~> ParsedAst.Predicate.Head.Positive
      }

      def Negative: Rule1[ParsedAst.Predicate.Head.Negative] = rule {
        SP ~ "!" ~ optWS ~ Names.QualifiedTable ~ optWS ~ NonEmptyArgumentList ~ SP ~> ParsedAst.Predicate.Head.Negative
      }
    }

    object Body {
      def Positive: Rule1[ParsedAst.Predicate.Body.Positive] = rule {
        SP ~ Names.QualifiedTable ~ optWS ~ NonEmptyPatternList ~ SP ~> ParsedAst.Predicate.Body.Positive
      }

      def Negative: Rule1[ParsedAst.Predicate.Body.Negative] = rule {
        SP ~ "!" ~ optWS ~ Names.QualifiedTable ~ optWS ~ NonEmptyPatternList ~ SP ~> ParsedAst.Predicate.Body.Negative
      }

      def Filter: Rule1[ParsedAst.Predicate.Body.Filter] = rule {
        SP ~ Names.QualifiedDefinition ~ optWS ~ NonEmptyArgumentList ~ SP ~> ParsedAst.Predicate.Body.Filter
      }

      def NotEqual: Rule1[ParsedAst.Predicate.Body.NotEqual] = rule {
        SP ~ Names.Variable ~ optWS ~ atomic("!=") ~ optWS ~ Names.Variable ~ SP ~> ParsedAst.Predicate.Body.NotEqual
      }

      def Loop: Rule1[ParsedAst.Predicate.Body.Loop] = rule {
        SP ~ Pattern ~ optWS ~ atomic("<-") ~ optWS ~ Expression ~ SP ~> ParsedAst.Predicate.Body.Loop
      }
    }

  }


  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def Type: Rule1[ParsedAst.Type] = rule {
    Types.UnaryArrow
  }

  object Types {

    def Infix: Rule1[ParsedAst.Type] = rule {
      Primary ~ optional(optWS ~ "`" ~ Ref ~ "`" ~ optWS ~ Primary ~ SP ~> ParsedAst.Type.Infix)
    }

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
      SP ~ Infix ~ optional(optWS ~ atomic("->") ~ optWS ~ Type) ~ SP ~> ((sp1: SourcePosition, t: ParsedAst.Type, o: Option[ParsedAst.Type], sp2: SourcePosition) => o match {
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
  def ArgumentList: Rule1[Seq[ParsedAst.Expression]] = rule {
    "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def NonEmptyArgumentList: Rule1[Seq[ParsedAst.Expression]] = rule {
    "(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def NonEmptyPatternList: Rule1[Seq[ParsedAst.Pattern]] = rule {
    "(" ~ optWS ~ oneOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def FormalParamList: Rule1[Seq[ParsedAst.FormalParam]] = rule {
    zeroOrMore(FormalParam).separatedBy(optWS ~ "," ~ optWS)
  }

  def FormalParam: Rule1[ParsedAst.FormalParam] = rule {
    SP ~ Names.Variable ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.FormalParam
  }

  def Annotation: Rule1[ParsedAst.AnnotationOrProperty] = rule {
    SP ~ atomic("@") ~ Names.Annotation ~ SP ~> ParsedAst.Annotation
  }

  def Property: Rule1[ParsedAst.AnnotationOrProperty] = {
    def ArgumentList: Rule1[Option[Seq[ParsedAst.Expression]]] = rule {
      optional("(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")")
    }

    rule {
      SP ~ atomic("#") ~ Names.QualifiedDefinition ~ ArgumentList ~ SP ~> ParsedAst.Property
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Names                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  object Names {

    /**
      * A lowercase letter.
      */
    val LowerLetter: CharPredicate = CharPredicate.LowerAlpha

    /**
      * An uppercase letter.
      */
    val UpperLetter: CharPredicate = CharPredicate.UpperAlpha

    /**
      * A greek letter.
      */
    val GreekLetter: CharPredicate = CharPredicate('\u0370' to '\u03FF')

    /**
      * A mathematical operator or arrow.
      */
    val MathLetter: CharPredicate =
      CharPredicate('\u2200' to '\u22FF') ++ // Mathematical Operator
        CharPredicate('\u2190' to '\u21FF') // Mathematical Arrow

    /**
      * An operator letter.
      */
    val OperatorLetter: CharPredicate = CharPredicate("+-*<>=!&|^")

    /**
      * a (upper/lower case) letter, numeral, greek letter, or other legal character.
      */
    val LegalLetter: CharPredicate = CharPredicate.AlphaNum ++ "_" ++ "'" ++ "!"

    /**
      * A lowercase identifier is a lowercase letter optionally followed by any letter, underscore, or prime.
      */
    def LowerCaseName: Rule1[Name.Ident] = rule {
      SP ~ capture(LowerLetter ~ zeroOrMore(LegalLetter)) ~ SP ~> Name.Ident
    }

    /**
      * An uppercase identifier is an uppercase letter optionally followed by any letter, underscore, or prime.
      */
    def UpperCaseName: Rule1[Name.Ident] = rule {
      SP ~ capture(UpperLetter ~ zeroOrMore(LegalLetter)) ~ SP ~> Name.Ident
    }

    /**
      * A lowercase qualified name is a namespace followed by a lowercase name.
      */
    def LowerCaseQName: Rule1[Name.QName] = rule {
      SP ~ optional(Namespace ~ ".") ~ LowerCaseName ~ SP ~> Name.QName.mk _
    }

    /**
      * An uppercase qualified name is a namespace followed by an uppercase name.
      */
    def UpperCaseQName: Rule1[Name.QName] = rule {
      SP ~ optional(Namespace ~ ".") ~ UpperCaseName ~ SP ~> Name.QName.mk _
    }

    /**
      * A greek identifier.
      */
    def GreekName: Rule1[Name.Ident] = rule {
      SP ~ capture(oneOrMore(GreekLetter)) ~ SP ~> Name.Ident
    }

    /**
      * A math identifier.
      */
    def MathName: Rule1[Name.Ident] = rule {
      SP ~ capture(oneOrMore(MathLetter)) ~ SP ~> Name.Ident
    }

    /**
      * An operator identifier.
      */
    def OperatorName: Rule1[Name.Ident] = rule {
      SP ~ capture(oneOrMore(OperatorLetter)) ~ SP ~> Name.Ident
    }

    /**
      * Namespaces are lower or uppercase.
      */
    def Namespace: Rule1[Name.NName] = rule {
      SP ~ oneOrMore(UpperCaseName).separatedBy("/") ~ SP ~>
        ((sp1: SourcePosition, parts: Seq[Name.Ident], sp2: SourcePosition) => Name.NName(sp1, parts.toList, sp2))
    }

    def Annotation: Rule1[Name.Ident] = LowerCaseName

    def Attribute: Rule1[Name.Ident] = LowerCaseName

    def Definition: Rule1[Name.Ident] = rule {
      LowerCaseName | GreekName | MathName | OperatorName
    }

    def QualifiedDefinition: Rule1[Name.QName] = LowerCaseQName // TODO: Greek letters?

    def Table: Rule1[Name.Ident] = UpperCaseName

    def QualifiedTable: Rule1[Name.QName] = UpperCaseQName

    def Tag: Rule1[Name.Ident] = UpperCaseName

    def QualifiedTag: Rule1[Name.QName] = UpperCaseQName

    def Type: Rule1[Name.Ident] = UpperCaseName

    def QualifiedType: Rule1[Name.QName] = UpperCaseQName

    def Variable: Rule1[Name.Ident] = rule {
      LowerCaseName | GreekName | MathName
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
