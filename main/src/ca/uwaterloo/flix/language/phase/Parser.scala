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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}
import org.parboiled2._

import scala.collection.immutable.Seq

/**
  * A phase to transform source files into abstract syntax trees.
  */
object Parser extends Phase[(List[Source], Map[Symbol.DefnSym, String]), ParsedAst.Program] {

  /**
    * Parses the given source inputs into an abstract syntax tree.
    */
  def run(arg: (List[Source], Map[Symbol.DefnSym, String]))(implicit flix: Flix): Validation[ParsedAst.Program, CompilationError] = flix.phase("Parser") {
    // The argument consists of a list of sources and the time spent by the reader.
    val (sources, namedExp) = arg

    // Retrieve the execution context.
    implicit val _ = flix.ec

    // Parse each source in parallel.
    val roots = sequence(ParOps.parMap(parseRoot, sources))

    // Parse each named expression.
    val named = traverse(namedExp) {
      case (sym, s) => parseExp(Source("<unknown>", s.toCharArray)).map(exp => sym -> exp)
    }

    // Sequence and combine the ASTs into one abstract syntax tree.
    mapN(roots, named) {
      case (as, ne) => ParsedAst.Program(as, ne.toMap)
    }

  }

  /**
    * Attempts to parse the given `source` as a root.
    */
  def parseRoot(source: Source): Validation[ParsedAst.Root, CompilationError] = {
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
    * Attempts to parse the given `source` as an expression.
    */
  def parseExp(source: Source): Validation[ParsedAst.Expression, CompilationError] = {
    val parser = new Parser(source)
    parser.Expression.run() match {
      case scala.util.Success(ast) =>
        ast.toSuccess
      case scala.util.Failure(e: org.parboiled2.ParseError) =>
        ca.uwaterloo.flix.language.errors.ParseError(parser.formatError(e), source).toFailure
      case scala.util.Failure(e) =>
        ca.uwaterloo.flix.language.errors.ParseError(e.getMessage, source).toFailure
    }
  }

}

/**
  * A parser for the Flix language.
  */
class Parser(val source: Source) extends org.parboiled2.Parser {

  /*
   * Initialize parser input.
   */
  override val input: ParserInput = new org.parboiled2.ParserInput.CharArrayBasedParserInput(source.data)

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
      SP ~ Imports ~ Decls ~ SP ~ optWS ~ EOI ~> ParsedAst.Root
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
      Declarations.Def |
      Declarations.Eff |
      Declarations.Handler |
      Declarations.Law |
      Declarations.Enum |
      Declarations.TypeDecl |
      Declarations.LatticeComponents |
      Declarations.Relation |
      Declarations.Lattice |
      Declarations.Class |
      Declarations.Impl |
      Declarations.Disallow
  }

  object Declarations {

    def Namespace: Rule1[ParsedAst.Declaration.Namespace] = rule {
      optWS ~ SP ~ atomic("namespace") ~ WS ~ Names.Namespace ~ optWS ~ '{' ~ zeroOrMore(Declaration) ~ optWS ~ '}' ~ SP ~> ParsedAst.Declaration.Namespace
    }

    def Def: Rule1[ParsedAst.Declaration.Def] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ atomic("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ optWS ~ "=" ~ optWS ~ Expressions.Statement ~ SP ~> ParsedAst.Declaration.Def
    }

    def Eff: Rule1[ParsedAst.Declaration.Eff] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ atomic("eff") ~ WS ~ Names.Eff ~ optWS ~ TypeParams ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ SP ~> ParsedAst.Declaration.Eff
    }

    def Handler: Rule1[ParsedAst.Declaration.Handler] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ atomic("handler") ~ WS ~ Names.Handler ~ optWS ~ TypeParams ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Declaration.Handler
    }

    def Sig: Rule1[ParsedAst.Declaration.Sig] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ atomic("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ SP ~> ParsedAst.Declaration.Sig
    }

    def Law: Rule1[ParsedAst.Declaration.Law] = rule {
      Documentation ~ SP ~ atomic("law") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Declaration.Law
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
        zeroOrMore(NestedCase | UnitCase).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        Documentation ~ Modifiers ~ SP ~ atomic("enum") ~ WS ~ Names.Type ~ TypeParams ~ optWS ~ "{" ~ optWS ~ Cases ~ optWS ~ "}" ~ SP ~> ParsedAst.Declaration.Enum
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
        Documentation ~ Modifiers ~ SP ~ atomic("type") ~ WS ~ Names.Type ~ WS ~ "=" ~ WS ~ (NestedCase | UnitCase) ~ SP ~> ParsedAst.Declaration.Type
      }
    }

    def Relation: Rule1[ParsedAst.Declaration.Relation] = rule {
      Documentation ~ Modifiers ~ SP ~ atomic("rel") ~ WS ~ Names.Predicate ~ optWS ~ TypeParams ~ AttributeList ~ SP ~> ParsedAst.Declaration.Relation
    }

    def Lattice: Rule1[ParsedAst.Declaration.Lattice] = rule {
      Documentation ~ Modifiers ~ SP ~ atomic("lat") ~ WS ~ Names.Predicate ~ optWS ~ TypeParams ~ AttributeList ~ SP ~> ParsedAst.Declaration.Lattice
    }

    def Constraint: Rule1[ParsedAst.Declaration.Constraint] = {
      def Head: Rule1[ParsedAst.Predicate.Head] = rule {
        HeadPredicate
      }

      def Body: Rule1[Seq[ParsedAst.Predicate.Body]] = rule {
        optional(optWS ~ ":-" ~ optWS ~ oneOrMore(BodyPredicate).separatedBy(optWS ~ "," ~ optWS)) ~> ((o: Option[Seq[ParsedAst.Predicate.Body]]) => o.getOrElse(Seq.empty))
      }

      rule {
        optWS ~ SP ~ Head ~ Body ~ optWS ~ "." ~ SP ~> ParsedAst.Declaration.Constraint
      }
    }

    def LatticeComponents: Rule1[ParsedAst.Declaration] = {
      def Elms: Rule1[Seq[ParsedAst.Expression]] = rule {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        optWS ~ SP ~ atomic("let") ~ optWS ~ Type ~ atomic("<>") ~ optWS ~ "=" ~ optWS ~ "(" ~ optWS ~ Elms ~ optWS ~ ")" ~ SP ~> ParsedAst.Declaration.LatticeComponents
      }
    }

    def Class: Rule1[ParsedAst.Declaration] = {
      def Head: Rule1[ParsedAst.SimpleClass] = SimpleClassAtom

      def Body: Rule1[Seq[ParsedAst.SimpleClass]] = rule {
        optional(optWS ~ atomic("<=") ~ optWS ~ oneOrMore(SimpleClassAtom).separatedBy(optWS ~ "," ~ optWS)) ~> (
          (o: Option[Seq[ParsedAst.SimpleClass]]) => o.getOrElse(Seq.empty))
      }

      def ClassConstraint: Rule1[ParsedAst.ClassConstraint] = rule {
        Head ~ optWS ~ Body ~> ParsedAst.ClassConstraint
      }

      def ClassBody: Rule1[Seq[ParsedAst.Declaration]] = rule {
        "{" ~ optWS ~ zeroOrMore(Declarations.Sig | Declarations.Law) ~ optWS ~ "}"
      }

      def ClassBodyOpt: Rule1[Seq[ParsedAst.Declaration]] = rule {
        optional(ClassBody) ~> ((o: Option[Seq[ParsedAst.Declaration]]) => o.getOrElse(Seq.empty))
      }

      rule {
        Documentation ~ SP ~ Modifiers ~ atomic("class") ~ WS ~ ClassConstraint ~ optWS ~ ClassBodyOpt ~ SP ~> ParsedAst.Declaration.Class
      }
    }

    def Impl: Rule1[ParsedAst.Declaration] = {
      def Head: Rule1[ParsedAst.ComplexClass] = PositiveClassAtom

      def Body: Rule1[Seq[ParsedAst.ComplexClass]] = rule {
        optional(atomic("<=") ~ WS ~ oneOrMore(PositiveClassAtom | NegativeClassAtom).separatedBy(optWS ~ "," ~ optWS)) ~> (
          (o: Option[Seq[ParsedAst.ComplexClass]]) => o.getOrElse(Seq.empty))
      }

      def ImplConstraint: Rule1[ParsedAst.ImplConstraint] = rule {
        Head ~ optWS ~ Body ~> ParsedAst.ImplConstraint
      }

      def ImplBody: Rule1[Seq[ParsedAst.Declaration.Def]] = rule {
        optional("{" ~ optWS ~ zeroOrMore(Declarations.Def).separatedBy(WS) ~ optWS ~ "}") ~> (
          (o: Option[Seq[ParsedAst.Declaration.Def]]) => o.getOrElse(Seq.empty))
      }

      rule {
        Documentation ~ SP ~ Modifiers ~ atomic("impl") ~ WS ~ ImplConstraint ~ optWS ~ ImplBody ~ SP ~> ParsedAst.Declaration.Impl
      }
    }

    def Disallow: Rule1[ParsedAst.Declaration] = {
      def IntegrityConstraint: Rule1[ParsedAst.DisallowConstraint] = rule {
        oneOrMore(PositiveClassAtom | NegativeClassAtom).separatedBy(optWS ~ "," ~ optWS) ~> ParsedAst.DisallowConstraint
      }

      rule {
        Documentation ~ SP ~ atomic("disallow") ~ WS ~ IntegrityConstraint ~ SP ~> ParsedAst.Declaration.Disallow
      }
    }

    private def SimpleClassAtom: Rule1[ParsedAst.SimpleClass] = rule {
      SP ~ Names.QualifiedClass ~ optWS ~ "[" ~ optWS ~ oneOrMore(Names.Variable).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.SimpleClass
    }

    private def PositiveClassAtom: Rule1[ParsedAst.ComplexClass.Positive] = rule {
      SP ~ Names.QualifiedClass ~ optWS ~ "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.ComplexClass.Positive
    }

    private def NegativeClassAtom: Rule1[ParsedAst.ComplexClass.Negative] = rule {
      SP ~ atomic("not") ~ WS ~ Names.QualifiedClass ~ optWS ~ "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.ComplexClass.Negative
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

    def Char: Rule1[ParsedAst.Literal.Char] = {
      def Normal: Rule1[String] = rule {
        capture(!"'" ~ !"\\" ~ CharPredicate.All)
      }

      def Special: Rule1[String] = rule {
        "\\\\" ~ push("\\") |
          "\\'" ~ push("'") |
          "\\n" ~ push("\n") |
          "\\r" ~ push("\r") |
          "\\t" ~ push("\t")
      }

      def Unicode: Rule1[String] = rule {
        "\\u" ~ capture(4 times CharPredicate.HexDigit) ~> ((x: String) =>
          // Convert the 4-digit string to a single character.
          Integer.parseInt(x, 16).toChar.toString)
      }

      rule {
        SP ~ "'" ~ (Normal | Special | Unicode) ~ "'" ~ SP ~> ParsedAst.Literal.Char
      }
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
    Expressions.Assign
  }

  object Expressions {
    def Statement: Rule1[ParsedAst.Expression] = rule {
      Expression ~ optional(optWS ~ atomic(";") ~ optWS ~ Statement ~ SP ~> ParsedAst.Expression.Statement)
    }

    def Assign: Rule1[ParsedAst.Expression] = rule {
      PutChannel ~ optional(optWS ~ atomic(":=") ~ optWS ~ PutChannel ~ SP ~> ParsedAst.Expression.Assign)
    }

    def PutChannel: Rule1[ParsedAst.Expression] = rule {
      LogicalOr ~ zeroOrMore(optWS ~ atomic("<-") ~ optWS ~ LogicalOr ~ SP ~> ParsedAst.Expression.PutChannel)
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
      Shift ~ optional(WS ~ capture(atomic("<=") | atomic(">=") | atomic("<") | atomic(">")) ~ WS ~ Shift ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Shift: Rule1[ParsedAst.Expression] = rule {
      Additive ~ optional(optWS ~ capture(atomic("<<<") | atomic(">>>")) ~ optWS ~ Additive ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Additive: Rule1[ParsedAst.Expression] = rule {
      Multiplicative ~ zeroOrMore(optWS ~ capture(atomic("+") | atomic("-")) ~ optWS ~ Multiplicative ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Multiplicative: Rule1[ParsedAst.Expression] = rule {
      Entails ~ zeroOrMore(optWS ~ capture(atomic("**") | atomic("*") | atomic("/") | atomic("%")) ~ optWS ~ Entails ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Entails: Rule1[ParsedAst.Expression] = rule {
      Compose ~ optional(optWS ~ atomic("|=") ~ optWS ~ Compose ~ SP ~> ParsedAst.Expression.FixpointEntails)
    }

    def Compose: Rule1[ParsedAst.Expression] = rule {
      Infix ~ zeroOrMore(optWS ~ atomic("<+>") ~ optWS ~ Infix ~ SP ~> ParsedAst.Expression.FixpointCompose)
    }

    def Infix: Rule1[ParsedAst.Expression] = rule {
      Special ~ zeroOrMore(optWS ~ "`" ~ Names.QualifiedDefinition ~ "`" ~ optWS ~ Special ~ SP ~> ParsedAst.Expression.Infix)
    }

    def Special: Rule1[ParsedAst.Expression] = {

      // NB: We allow any operator, other than a reserved operator, to be matched by this rule.
      def Reserved2: Rule1[String] = rule {
        capture("**" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "=>" | "->" | "<-" | "|=")
      }

      // NB: We allow any operator, other than a reserved operator, to be matched by this rule.
      def Reserved3: Rule1[String] = rule {
        capture("<<<" | ">>>" | "<+>")
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
      !Literal ~ (SP ~ capture(atomic("!") | atomic("+") | atomic("-") | atomic("~~~")) ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Unary) | Ref
    }

    def Ref: Rule1[ParsedAst.Expression] = rule {
      (SP ~ atomic("ref") ~ WS ~ Ref ~ SP ~> ParsedAst.Expression.Ref) | Deref
    }

    def Deref: Rule1[ParsedAst.Expression] = rule {
      (SP ~ atomic("deref") ~ WS ~ Deref ~ SP ~> ParsedAst.Expression.Deref) | Cast
    }

    def Cast: Rule1[ParsedAst.Expression] = rule {
      Ascribe ~ optional(WS ~ atomic("as") ~ WS ~ TypeAndEffect ~ SP ~> ParsedAst.Expression.Cast)
    }

    def Ascribe: Rule1[ParsedAst.Expression] = rule {
      FAppend ~ optional(optWS ~ ":" ~ optWS ~ TypeAndEffect ~ SP ~> ParsedAst.Expression.Ascribe)
    }

    def Primary: Rule1[ParsedAst.Expression] = rule {
      LetRec | LetMatch | IfThenElse | Match | LambdaMatch | Switch | TryCatch | Native | Lambda | Tuple |
        RecordOperation | RecordLiteral | Block | RecordSelectLambda | NewChannel |
        GetChannel | SelectChannel | Spawn | Sleep | ArrayLit | ArrayNew | ArrayLength |
        VectorLit | VectorNew | VectorLength | FNil | FSet | FMap | FixpointSolve |
        FixpointProject | ConstraintSeq | Literal | HandleWith | Existential | Universal |
        UnaryLambda | QName | Tag | SName | Hole
    }

    def Literal: Rule1[ParsedAst.Expression.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Expression.Lit
    }

    def IfThenElse: Rule1[ParsedAst.Expression.IfThenElse] = rule {
      SP ~ atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ WS ~ atomic("else") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
    }

    def LetRec: Rule1[ParsedAst.Expression.LetRec] = rule {
      SP ~ atomic("letrec") ~ WS ~ Names.Variable ~ optWS ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Statement ~ SP ~> ParsedAst.Expression.LetRec
    }

    def LetMatch: Rule1[ParsedAst.Expression.LetMatch] = rule {
      SP ~ atomic("let") ~ WS ~ Pattern ~ optWS ~ optional(":" ~ optWS ~ Type ~ optWS) ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Statement ~ SP ~> ParsedAst.Expression.LetMatch
    }

    def Match: Rule1[ParsedAst.Expression.Match] = {
      def Rule: Rule1[ParsedAst.MatchRule] = rule {
        atomic("case") ~ WS ~ Pattern ~ optWS ~ optional(atomic("if") ~ WS ~ Expression ~ optWS) ~ atomic("=>") ~ optWS ~ Statement ~> ParsedAst.MatchRule
      }

      rule {
        SP ~ atomic("match") ~ WS ~ Expression ~ WS ~ atomic("with") ~ WS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(WS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Match
      }
    }

    def Switch: Rule1[ParsedAst.Expression.Switch] = {
      def Rule: Rule1[(ParsedAst.Expression, ParsedAst.Expression)] = rule {
        atomic("case") ~ WS ~ Expression ~ optWS ~ "=>" ~ optWS ~ Statement ~> ((e1: ParsedAst.Expression, e2: ParsedAst.Expression) => (e1, e2))
      }

      rule {
        SP ~ atomic("switch") ~ WS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Switch
      }
    }

    def TryCatch: Rule1[ParsedAst.Expression] = {
      def CatchRule: Rule1[ParsedAst.CatchRule] = rule {
        atomic("case") ~ WS ~ Names.Variable ~ optWS ~ ":" ~ optWS ~ atomic("##") ~ Names.JavaName ~ WS ~ atomic("=>") ~ optWS ~ Expression ~> ParsedAst.CatchRule
      }

      def CatchBody: Rule1[Seq[ParsedAst.CatchRule]] = rule {
        "{" ~ optWS ~ oneOrMore(CatchRule).separatedBy(WS) ~ optWS ~ "}"
      }

      rule {
        SP ~ atomic("try") ~ WS ~ Expression ~ optWS ~ atomic("catch") ~ optWS ~ CatchBody ~ SP ~> ParsedAst.Expression.TryCatch
      }
    }

    def Native: Rule1[ParsedAst.Expression] = {
      def NativeConstructor: Rule1[ParsedAst.Expression.NativeConstructor] = rule {
        atomic("new") ~ WS ~ SP ~ Names.JavaName ~ optWS ~ ArgumentList ~ SP ~> ParsedAst.Expression.NativeConstructor
      }

      def NativeField: Rule1[ParsedAst.Expression.NativeField] = rule {
        atomic("field") ~ WS ~ SP ~ Names.JavaName ~ SP ~> ParsedAst.Expression.NativeField
      }

      def NativeMethod: Rule1[ParsedAst.Expression.NativeMethod] = rule {
        atomic("method") ~ WS ~ SP ~ Names.JavaName ~ optWS ~ ArgumentList ~ SP ~> ParsedAst.Expression.NativeMethod
      }

      rule {
        atomic("native") ~ WS ~ (NativeField | NativeMethod | NativeConstructor)
      }
    }

    def RecordSelect: Rule1[ParsedAst.Expression] = rule {
      Postfix ~ zeroOrMore(optWS ~ "." ~ Names.Field ~ SP ~> ParsedAst.Expression.RecordSelect)
    }

    //TODO SJ: order this with primaries
    def NewChannel: Rule1[ParsedAst.Expression.NewChannel] = rule {
      SP ~ atomic("chan") ~ WS ~ Type ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.NewChannel
    }

    def GetChannel: Rule1[ParsedAst.Expression.GetChannel] = rule {
      SP ~ atomic("<-") ~ WS ~ RecordSelect ~ SP ~> ParsedAst.Expression.GetChannel
    }

    def SelectChannel: Rule1[ParsedAst.Expression.SelectChannel] = {
      def SelectChannelRule: Rule1[ParsedAst.SelectChannelRule] = rule {
        atomic("case") ~ WS ~ Names.Variable ~ optWS ~ atomic("<-") ~ optWS ~ Expression ~ optWS ~ atomic("=>") ~ optWS ~ Statement ~> ParsedAst.SelectChannelRule
      }

      def SelectChannelDefault: Rule1[ParsedAst.Expression] = rule {
        atomic("case") ~ WS ~ atomic("_") ~ optWS ~ atomic("=>") ~ optWS ~ Statement
      }

      rule {
        SP ~ atomic("select") ~ WS ~ "{" ~ optWS ~ oneOrMore(SelectChannelRule).separatedBy(optWS) ~ optWS ~ optional(SelectChannelDefault) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.SelectChannel
      }
    }

    def Spawn: Rule1[ParsedAst.Expression.Spawn] = rule {
      SP ~ atomic("spawn") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Spawn
    }

    def Sleep: Rule1[ParsedAst.Expression.Sleep] = rule {
      SP ~ atomic("sleep") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Sleep
    }

    def Postfix: Rule1[ParsedAst.Expression] = rule {
      ArraySlice ~ zeroOrMore(optWS ~ "." ~ Names.Definition ~ ArgumentList ~ SP ~> ParsedAst.Expression.Postfix)
    }

    def ArraySlice: Rule1[ParsedAst.Expression] = rule {
      ArrayLoad ~ optional(optWS ~ "[" ~ optWS ~ optional(Expression) ~ optWS ~ atomic("..") ~ optWS ~ optional(Expression) ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArraySlice)
    }

    def ArrayLoad: Rule1[ParsedAst.Expression] = rule {
      ArrayStore ~ zeroOrMore(optWS ~ "[" ~ optWS ~ Expression ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArrayLoad)
    }

    def ArrayStore: Rule1[ParsedAst.Expression] = rule {
      VectorSlice ~ optional(oneOrMore(optWS ~ "[" ~ optWS ~ Expression ~ optWS ~ "]") ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.ArrayStore)
    }

    def VectorSlice: Rule1[ParsedAst.Expression] = rule {
      VectorLoad ~ optional(optWS ~ atomic("[|") ~ optWS ~ optional(Literals.IntDefault) ~ optWS ~ atomic("..") ~ optWS ~ optional(Literals.IntDefault) ~ optWS ~ atomic("|]") ~ SP ~> ParsedAst.Expression.VectorSlice)
    }

    def VectorLoad: Rule1[ParsedAst.Expression] = rule {
      VectorStore ~ zeroOrMore(optWS ~ atomic("[|") ~ optWS ~ Literals.IntDefault ~ optWS ~ atomic("|]") ~ SP ~> ParsedAst.Expression.VectorLoad)
    }

    def VectorStore: Rule1[ParsedAst.Expression] = rule {
      Apply ~ optional(oneOrMore(optWS ~ atomic("[|") ~ optWS ~ Literals.IntDefault ~ optWS ~ atomic("|]")) ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.VectorStore)
    }

    def Apply: Rule1[ParsedAst.Expression] = rule {
      Primary ~ zeroOrMore(ArgumentList ~ SP ~> ParsedAst.Expression.Apply)
    }

    def Tag: Rule1[ParsedAst.Expression.Tag] = rule {
      SP ~ Names.QualifiedTag ~ optional(optWS ~ Tuple) ~ SP ~> ParsedAst.Expression.Tag
    }

    def Tuple: Rule1[ParsedAst.Expression] = rule {
      SP ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Tuple
    }

    def Block: Rule1[ParsedAst.Expression] = rule {
      "{" ~ optWS ~ Statement ~ optWS ~ "}"
    }

    def RecordLiteral: Rule1[ParsedAst.Expression] = {
      def FieldLit: Rule1[ParsedAst.RecordField] = rule {
        SP ~ Names.Field ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.RecordField
      }

      rule {
        SP ~ "{" ~ optWS ~ zeroOrMore(FieldLit).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.RecordLit
      }
    }

    def RecordSelectLambda: Rule1[ParsedAst.Expression] = rule {
      SP ~ "." ~ Names.Field ~ SP ~> ParsedAst.Expression.RecordSelectLambda
    }

    def RecordOperation: Rule1[ParsedAst.Expression] = {
      def RecordOp: Rule1[ParsedAst.RecordOp] = rule {
        Extend | Restrict | Update
      }

      def Extend: Rule1[ParsedAst.RecordOp.Extend] = rule {
        SP ~ "+" ~ Names.Field ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.RecordOp.Extend
      }

      def Restrict: Rule1[ParsedAst.RecordOp.Restrict] = rule {
        SP ~ "-" ~ Names.Field ~ SP ~> ParsedAst.RecordOp.Restrict
      }

      def Update: Rule1[ParsedAst.RecordOp.Update] = rule {
        SP ~ Names.Field ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.RecordOp.Update
      }

      rule {
        SP ~ "{" ~ optWS ~ oneOrMore(RecordOp).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ atomic("|") ~ optWS ~ Expression ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.RecordOperation
      }
    }

    def ArrayLit: Rule1[ParsedAst.Expression] = rule {
      SP ~ "[" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArrayLit
    }

    def ArrayNew: Rule1[ParsedAst.Expression] = rule {
      SP ~ "[" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Expression ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArrayNew
    }

    def ArrayLength: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("length") ~ optWS ~ "[" ~ optWS ~ Expression ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArrayLength
    }

    def VectorLit: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("[|") ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "|]" ~ SP ~> ParsedAst.Expression.VectorLit
    }

    def VectorNew: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("[|") ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Literals.IntDefault ~ optWS ~ "|]" ~ SP ~> ParsedAst.Expression.VectorNew
    }

    def VectorLength: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("length") ~ optWS ~ atomic("[|") ~ optWS ~ Expression ~ optWS ~ "|]" ~ SP ~> ParsedAst.Expression.VectorLength
    }

    def FNil: Rule1[ParsedAst.Expression.FNil] = rule {
      SP ~ atomic("Nil") ~ SP ~> ParsedAst.Expression.FNil
    }

    def FAppend: Rule1[ParsedAst.Expression] = rule {
      FList ~ optional(optWS ~ SP ~ atomic(":::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FAppend)
    }

    def FList: Rule1[ParsedAst.Expression] = rule {
      RecordSelect ~ optional(optWS ~ SP ~ atomic("::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FCons)
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

    def SName: Rule1[ParsedAst.Expression.SName] = rule {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Expression.SName
    }

    def QName: Rule1[ParsedAst.Expression.QName] = rule {
      SP ~ Names.QualifiedDefinition ~ SP ~> ParsedAst.Expression.QName
    }

    def Hole: Rule1[ParsedAst.Expression.Hole] = {
      def AnonymousHole: Rule1[ParsedAst.Expression.Hole] = rule {
        SP ~ atomic("???") ~ SP ~> ((sp1: SourcePosition, sp2: SourcePosition) => ParsedAst.Expression.Hole(sp1, None, sp2))
      }

      def NamedHole: Rule1[ParsedAst.Expression.Hole] = rule {
        SP ~ atomic("?") ~ Names.Hole ~ SP ~> ((sp1: SourcePosition, name: Name.Ident, sp2: SourcePosition) => ParsedAst.Expression.Hole(sp1, Some(name), sp2))
      }

      rule {
        AnonymousHole | NamedHole
      }
    }

    def UnaryLambda: Rule1[ParsedAst.Expression.Lambda] = rule {
      SP ~ FormalParam ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ((sp1: SourcePosition, param: ParsedAst.FormalParam, body: ParsedAst.Expression, sp2: SourcePosition) =>
        ParsedAst.Expression.Lambda(sp1, Seq(param), body, sp2))
    }

    def Lambda: Rule1[ParsedAst.Expression.Lambda] = rule {
      SP ~ FormalParamList ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Lambda
    }

    def LambdaMatch: Rule1[ParsedAst.Expression.LambdaMatch] = rule {
      SP ~ atomic("match") ~ optWS ~ Pattern ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.LambdaMatch
    }

    def ConstraintSeq: Rule1[ParsedAst.Expression] = rule {
      SP ~ oneOrMore(Declarations.Constraint) ~ SP ~> ParsedAst.Expression.FixpointConstraintSeq
    }

    def FixpointSolve: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("solve") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.FixpointSolve
    }

    def FixpointProject: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("project") ~ WS ~ Names.QualifiedPredicate ~ optional("<" ~ Expressions.Primary ~ ">") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.FixpointProject
    }

    def HandleWith: Rule1[ParsedAst.Expression.HandleWith] = {
      def EffectHandler: Rule1[ParsedAst.HandlerBinding] = rule {
        atomic("eff") ~ WS ~ Names.QualifiedEffect ~ optWS ~ "=" ~ optWS ~ Expression ~> ParsedAst.HandlerBinding
      }

      def HandlerBody: Rule1[Seq[ParsedAst.HandlerBinding]] = rule {
        atomic("{") ~ optWS ~ oneOrMore(EffectHandler).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ atomic("}")
      }

      rule {
        // Decide on the name...
        SP ~ atomic("do") ~ WS ~ Expression ~ WS ~ atomic("with") ~ WS ~ HandlerBody ~ SP ~> ParsedAst.Expression.HandleWith
      }
    }

    def Existential: Rule1[ParsedAst.Expression.Existential] = rule {
      SP ~ atomic("∃" | "\\exists") ~ optWS ~ FormalParamList ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Existential
    }

    def Universal: Rule1[ParsedAst.Expression.Universal] = rule {
      SP ~ atomic("∀" | "\\forall") ~ optWS ~ FormalParamList ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Universal
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
      FNil | Tag | Lit | Tuple | Var
    }

    def Var: Rule1[ParsedAst.Pattern.Var] = rule {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Pattern.Var
    }

    def Lit: Rule1[ParsedAst.Pattern.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Pattern.Lit
    }

    def Tag: Rule1[ParsedAst.Pattern.Tag] = rule {
      SP ~ Names.QualifiedTag ~ optional(optWS ~ Tuple) ~ SP ~> ParsedAst.Pattern.Tag
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

  }

  /////////////////////////////////////////////////////////////////////////////
  // Predicates                                                              //
  /////////////////////////////////////////////////////////////////////////////
  def HeadPredicate: Rule1[ParsedAst.Predicate.Head] = rule {
    Predicates.Head.Atom
  }

  def BodyPredicate: Rule1[ParsedAst.Predicate.Body] = rule {
    Predicates.Body.Positive | Predicates.Body.Negative | Predicates.Body.Filter | Predicates.Body.ApplyFilter | Predicates.Body.NotEqual | Predicates.Body.Loop
  }

  object Predicates {

    object Head {

      def Atom: Rule1[ParsedAst.Predicate.Head.Atom] = rule {
        SP ~ Names.QualifiedPredicate ~ optional("<" ~ Expressions.Primary ~ ">") ~ optWS ~ ArgumentList ~ SP ~> ParsedAst.Predicate.Head.Atom
      }

    }

    object Body {
      def Positive: Rule1[ParsedAst.Predicate.Body.Positive] = rule {
        SP ~ Names.QualifiedPredicate ~ optional("<" ~ Expressions.Primary ~ ">") ~ optWS ~ PatternList ~ SP ~> ParsedAst.Predicate.Body.Positive
      }

      def Negative: Rule1[ParsedAst.Predicate.Body.Negative] = {
        def Not: Rule0 = rule {
          "!" | (atomic("not") ~ WS)
        }

        rule {
          SP ~ Not ~ optWS ~ Names.QualifiedPredicate ~ optional("<" ~ Expressions.Primary ~ ">") ~ optWS ~ PatternList ~ SP ~> ParsedAst.Predicate.Body.Negative
        }
      }

      def Filter: Rule1[ParsedAst.Predicate.Body.Filter] = rule {
        SP ~ atomic("if") ~ WS ~ Expression ~ SP ~> ParsedAst.Predicate.Body.Filter
      }

      def ApplyFilter: Rule1[ParsedAst.Predicate.Body.ApplyFilter] = rule {
        SP ~ Names.QualifiedDefinition ~ optWS ~ ArgumentList ~ SP ~> ParsedAst.Predicate.Body.ApplyFilter
      }

      def NotEqual: Rule1[ParsedAst.Predicate.Body.NotEqual] = rule {
        SP ~ Names.Variable ~ optWS ~ atomic("!=") ~ optWS ~ Names.Variable ~ SP ~> ParsedAst.Predicate.Body.NotEqual
      }

      def Loop: Rule1[ParsedAst.Predicate.Body.Functional] = rule {
        SP ~ Names.Variable ~ optWS ~ atomic("<-") ~ optWS ~ Expression ~ SP ~> ParsedAst.Predicate.Body.Functional
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

    def UnaryArrow: Rule1[ParsedAst.Type] = rule {
      SP ~ Infix ~ optional(optWS ~ atomic("->") ~ optWS ~ Type) ~ SP ~> ((sp1: SourcePosition, t: ParsedAst.Type, o: Option[ParsedAst.Type], sp2: SourcePosition) => o match {
        case None => t
        case Some(r) => ParsedAst.Type.Arrow(sp1, List(t), r, sp2) // TODO: Maybe need to reverse order???
      })
    }

    def Infix: Rule1[ParsedAst.Type] = rule {
      Apply ~ optional(optWS ~ "`" ~ Ambiguous ~ "`" ~ optWS ~ Apply ~ SP ~> ParsedAst.Type.Infix)
    }

    def Apply: Rule1[ParsedAst.Type] = rule {
      Primary ~ zeroOrMore(TypeArguments ~ SP ~> ParsedAst.Type.Apply)
    }

    def Primary: Rule1[ParsedAst.Type] = rule {
      Arrow | Nat | Tuple | Record | Schema | Native | Var | Ambiguous
    }

    def Arrow: Rule1[ParsedAst.Type] = rule {
      SP ~ "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~ atomic("->") ~ optWS ~ Type ~ SP ~> ParsedAst.Type.Arrow
    }

    def Nat: Rule1[ParsedAst.Type] = rule {
      SP ~ Literals.IntDefault ~ SP ~> ParsedAst.Type.Nat
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

    def Record: Rule1[ParsedAst.Type] = {
      def RecordFieldType: Rule1[ParsedAst.RecordFieldType] = rule {
        SP ~ Names.Field ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.RecordFieldType
      }

      rule {
        SP ~ atomic("{") ~ optWS ~ zeroOrMore(RecordFieldType).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ "}" ~ SP ~> ParsedAst.Type.Record
      }
    }

    def Schema: Rule1[ParsedAst.Type] = rule {
      SP ~ atomic("Schema") ~ optWS ~ atomic("{") ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ "}" ~ SP ~> ParsedAst.Type.Schema
    }

    def Native: Rule1[ParsedAst.Type] = rule {
      SP ~ atomic("##") ~ Names.JavaName ~ SP ~> ParsedAst.Type.Native
    }

    def Var: Rule1[ParsedAst.Type] = rule {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Type.Var
    }

    def Ambiguous: Rule1[ParsedAst.Type] = rule {
      SP ~ Names.QualifiedType ~ SP ~> ParsedAst.Type.Ambiguous
    }

    def TypeArguments: Rule1[Seq[ParsedAst.Type]] = rule {
      "[" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]"
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Effects                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  def Effect: Rule1[ParsedAst.Effect] = rule {
    oneOrMore(Names.Effect).separatedBy(optWS ~ "," ~ optWS) ~> ParsedAst.Effect
  }

  /////////////////////////////////////////////////////////////////////////////
  // Type and (optional) Effects                                             //
  /////////////////////////////////////////////////////////////////////////////
  def TypeAndEffect: Rule2[ParsedAst.Type, Option[ParsedAst.Effect]] = rule {
    Type ~ optional(optWS ~ atomic("@") ~ WS ~ Effect)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Helpers                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  def FormalParam: Rule1[ParsedAst.FormalParam] = rule {
    SP ~ Modifiers ~ Names.Variable ~ optional(optWS ~ ":" ~ optWS ~ Type) ~ SP ~> ParsedAst.FormalParam
  }

  def FormalParamList: Rule1[Seq[ParsedAst.FormalParam]] = rule {
    "(" ~ optWS ~ zeroOrMore(FormalParam).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def ArgumentList: Rule1[Seq[ParsedAst.Expression]] = rule {
    "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def PatternList: Rule1[Seq[ParsedAst.Pattern]] = rule {
    "(" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def AttributeList: Rule1[Seq[ParsedAst.Attribute]] = rule {
    "(" ~ optWS ~ zeroOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def Annotations: Rule1[Seq[ParsedAst.AnnotationOrProperty]] = rule {
    zeroOrMore(Annotation | Property).separatedBy(WS) ~ optWS
  }

  def Modifiers: Rule1[Seq[ParsedAst.Modifier]] = {
    def Inline: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(atomic("inline")) ~ SP ~> ParsedAst.Modifier
    }

    def Public: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(atomic("pub")) ~ SP ~> ParsedAst.Modifier
    }

    def Modifier: Rule1[ParsedAst.Modifier] = rule {
      Inline | Public
    }

    rule {
      zeroOrMore(Modifier).separatedBy(WS) ~ optWS
    }
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
    val LowerLetter: CharPredicate = CharPredicate.LowerAlpha ++ "_"

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
    val LegalLetter: CharPredicate = CharPredicate.AlphaNum ++ "_" ++ "!"

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

    def Class: Rule1[Name.Ident] = UpperCaseName

    def QualifiedClass: Rule1[Name.QName] = UpperCaseQName

    def Definition: Rule1[Name.Ident] = rule {
      LowerCaseName | GreekName | MathName | OperatorName
    }

    def Eff: Rule1[Name.Ident] = LowerCaseName

    def Handler: Rule1[Name.Ident] = LowerCaseName

    def Effect: Rule1[Name.Ident] = UpperCaseName

    def Field: Rule1[Name.Ident] = LowerCaseName

    def Hole: Rule1[Name.Ident] = LowerCaseName

    def QualifiedDefinition: Rule1[Name.QName] = LowerCaseQName // TODO: Greek letters?

    def QualifiedEffect: Rule1[Name.QName] = LowerCaseQName

    def Predicate: Rule1[Name.Ident] = UpperCaseName

    def QualifiedPredicate: Rule1[Name.QName] = UpperCaseQName

    def Tag: Rule1[Name.Ident] = UpperCaseName

    def QualifiedTag: Rule1[Name.QName] = UpperCaseQName

    def Type: Rule1[Name.Ident] = UpperCaseName

    def QualifiedType: Rule1[Name.QName] = UpperCaseQName

    def Variable: Rule1[Name.Ident] = rule {
      LowerCaseName | GreekName | MathName
    }

    def JavaName: Rule1[Seq[String]] = {
      def JavaIdentifier: Rule1[String] = rule {
        capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum))
      }

      rule {
        oneOrMore(JavaIdentifier).separatedBy(".")
      }
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
  def Documentation: Rule1[ParsedAst.Doc] = {
    // Matches real whitespace.
    def PureWS: Rule0 = rule {
      zeroOrMore(" " | "\t" | NewLine)
    }

    // Matches triple dashed comments.
    def TripleSlash: Rule1[Seq[String]] = rule {
      oneOrMore(PureWS ~ "///" ~ capture(zeroOrMore(!NewLine ~ ANY)) ~ (NewLine | EOI))
    }

    // Optionally matches a triple dashed comment and then any whitespace.
    rule {
      SP ~ optional(TripleSlash) ~ SP ~ optWS ~> (
        (sp1: SourcePosition, o: Option[Seq[String]], sp2: SourcePosition) => o match {
          case None => ParsedAst.Doc(sp1, Seq.empty, sp2)
          case Some(lines) => ParsedAst.Doc(sp1, lines, sp2)
        })
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
