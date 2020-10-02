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
import ca.uwaterloo.flix.language.ast.Ast.{Polarity, Source}
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}
import org.parboiled2._

import scala.collection.immutable.Seq

/**
  * A phase to transform source files into abstract syntax trees.
  */
object Parser extends Phase[List[Source], ParsedAst.Program] {

  /**
    * Parses the given source inputs into an abstract syntax tree.
    */
  def run(sources: List[Source])(implicit flix: Flix): Validation[ParsedAst.Program, CompilationError] = flix.phase("Parser") {
    // Parse each source in parallel.
    val roots = sequence(ParOps.parMap(sources, parseRoot))

    // Sequence and combine the ASTs into one abstract syntax tree.
    mapN(roots) {
      case as => ParsedAst.Program(as)
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
        val loc = SourceLocation(source, e.position.line, e.position.column, e.position.line, e.position.column, _ => "")
        ca.uwaterloo.flix.language.errors.ParseError(parser.formatError(e), loc).toFailure
      case scala.util.Failure(e) =>
        ca.uwaterloo.flix.language.errors.ParseError(e.getMessage, SourceLocation.Unknown).toFailure
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
        val loc = SourceLocation(source, e.position.line, e.position.column, e.position.line, e.position.column, _ => "")
        ca.uwaterloo.flix.language.errors.ParseError(parser.formatError(e), loc).toFailure
      case scala.util.Failure(e) =>
        ca.uwaterloo.flix.language.errors.ParseError(e.getMessage, SourceLocation.Unknown).toFailure
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
    def Uses: Rule1[Seq[ParsedAst.Use]] = rule {
      zeroOrMore(Use ~ optWS ~ ";").separatedBy(optWS)
    }

    def Decls: Rule1[Seq[ParsedAst.Declaration]] = rule {
      zeroOrMore(Declaration)
    }

    rule {
      optWS ~ SP ~ Uses ~ Decls ~ SP ~ optWS ~ EOI ~> ParsedAst.Root
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations                                                            //
  /////////////////////////////////////////////////////////////////////////////
  def Declaration: Rule1[ParsedAst.Declaration] = rule {
    Declarations.Namespace |
      Declarations.Constraint |
      Declarations.Def |
      Declarations.Law |
      Declarations.Enum |
      Declarations.OpaqueType |
      Declarations.TypeAlias |
      Declarations.LatticeComponents |
      Declarations.Relation |
      Declarations.Lattice |
      Declarations.Class
  }

  object Declarations {

    def Namespace: Rule1[ParsedAst.Declaration.Namespace] = {
      def Uses: Rule1[Seq[ParsedAst.Use]] = rule {
        zeroOrMore(Use ~ optWS ~ ";").separatedBy(optWS)
      }

      def Decls: Rule1[Seq[ParsedAst.Declaration]] = rule {
        zeroOrMore(Declaration)
      }

      rule {
        optWS ~ SP ~ keyword("namespace") ~ WS ~ Names.Namespace ~ optWS ~ '{' ~ optWS ~ Uses ~ Decls ~ optWS ~ '}' ~ SP ~> ParsedAst.Declaration.Namespace
      }
    }

    def Def: Rule1[ParsedAst.Declaration.Def] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ optWS ~ "=" ~ optWS ~ Expressions.Stm ~ SP ~> ParsedAst.Declaration.Def
    }

    def Sig: Rule1[ParsedAst.Declaration.Sig] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ SP ~> ParsedAst.Declaration.Sig
    }

    def Law: Rule1[ParsedAst.Declaration.Law] = rule {
      Documentation ~ SP ~ keyword("law") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Declaration.Law
    }

    def Enum: Rule1[ParsedAst.Declaration.Enum] = {
      def Case: Rule1[ParsedAst.Case] = {
        def CaseWithUnit: Rule1[ParsedAst.Case] = rule {
          SP ~ Names.Tag ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) =>
            ParsedAst.Case(sp1, ident, ParsedAst.Type.Unit(sp1, sp2), sp2))
        }

        def CaseWithType: Rule1[ParsedAst.Case] = rule {
          SP ~ Names.Tag ~ Type ~ SP ~> ParsedAst.Case
        }

        rule {
          CaseWithType | CaseWithUnit
        }
      }

      def NonEmptyCaseList: Rule1[Seq[ParsedAst.Case]] = rule {
        // Note: We use the case keyword as part of the separator with or without a comma.
        keyword("case") ~ WS ~ oneOrMore(Case).separatedBy(
          (optWS ~ "," ~ optWS ~ keyword("case") ~ WS) | (WS ~ keyword("case") ~ WS) | (optWS ~ "," ~ optWS)
        )
      }

      def Cases: Rule1[Seq[ParsedAst.Case]] = rule {
        optional(NonEmptyCaseList) ~> ((xs: Option[Seq[ParsedAst.Case]]) => xs.getOrElse(Seq.empty))
      }

      rule {
        Documentation ~ Modifiers ~ SP ~ keyword("enum") ~ WS ~ Names.Type ~ TypeParams ~ optWS ~ "{" ~ optWS ~ Cases ~ optWS ~ "}" ~ SP ~> ParsedAst.Declaration.Enum
      }
    }

    def OpaqueType: Rule1[ParsedAst.Declaration.OpaqueType] = rule {
      Documentation ~ Modifiers ~ SP ~ keyword("opaque") ~ WS ~ keyword("type") ~ WS ~ Names.Type ~ optWS ~ TypeParams ~ optWS ~ "=" ~ optWS ~ Type ~ SP ~> ParsedAst.Declaration.OpaqueType
    }

    def TypeAlias: Rule1[ParsedAst.Declaration.TypeAlias] = rule {
      Documentation ~ Modifiers ~ SP ~ keyword("type") ~ WS ~ keyword("alias") ~ WS ~ Names.Type ~ optWS ~ TypeParams ~ optWS ~ "=" ~ optWS ~ Type ~ SP ~> ParsedAst.Declaration.TypeAlias
    }

    def Relation: Rule1[ParsedAst.Declaration.Relation] = rule {
      Documentation ~ Modifiers ~ SP ~ keyword("rel") ~ WS ~ Names.Predicate ~ optWS ~ TypeParams ~ AttributeList ~ SP ~> ParsedAst.Declaration.Relation
    }

    def Lattice: Rule1[ParsedAst.Declaration.Lattice] = rule {
      Documentation ~ Modifiers ~ SP ~ keyword("lat") ~ WS ~ Names.Predicate ~ optWS ~ TypeParams ~ AttributeList ~ SP ~> ParsedAst.Declaration.Lattice
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
        optWS ~ SP ~ keyword("let") ~ optWS ~ Type ~ atomic("<>") ~ optWS ~ "=" ~ optWS ~ "(" ~ optWS ~ Elms ~ optWS ~ ")" ~ SP ~> ParsedAst.Declaration.LatticeComponents
      }
    }

    def Class: Rule1[ParsedAst.Declaration] = {
      def Head = rule {
        Documentation ~ Modifiers ~ SP ~ keyword("class") ~ WS ~ Names.Class ~ optWS ~ TypeParams
      }

      def EmptyBody = rule {
        push(Nil) ~ SP
      }

      def NonEmptyBody = rule {
        optWS ~ "{" ~ optWS ~ zeroOrMore(Declarations.Sig).separatedBy(WS) ~ optWS ~ "}" ~ SP
      }

      rule {
        Head ~ (NonEmptyBody | EmptyBody) ~> ParsedAst.Declaration.Class
      }
    }

    def Instance: Rule1[ParsedAst.Declaration] = {
      def Head = rule {
        Documentation ~ Modifiers ~ SP ~ keyword("instance") ~ WS ~ Names.QualifiedClass ~ optWS ~ "[" ~ optWS ~ Type ~ optWS ~ "]"
      }

      def EmptyBody = rule {
        push(Nil) ~ SP
      }

      def NonEmptyBody = rule {
        optWS ~ "{" ~ optWS ~ zeroOrMore(Declarations.Def).separatedBy(WS) ~ optWS ~ "}" ~ SP
      }

      rule {
        Head ~ (NonEmptyBody | EmptyBody) ~> ParsedAst.Declaration.Instance
      }
    }

    def TypeParams: Rule1[ParsedAst.TypeParams] = {
      def ConstrainedType: Rule1[ParsedAst.ConstrainedType] = rule {
        SP ~ Names.Variable ~ optional(optWS ~ ":" ~ optWS ~ Names.QualifiedClass) ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, bound: Option[Name.QName], sp2: SourcePosition) => bound match {
          case None => ParsedAst.ConstrainedType(sp1, ident, Seq.empty, sp2)
          case Some(clazz) => ParsedAst.ConstrainedType(sp1, ident, Seq(clazz), sp2)
        })
      }

      rule {
        optional("[" ~ optWS ~ oneOrMore(ConstrainedType).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]") ~> ((o: Option[Seq[ParsedAst.ConstrainedType]]) => o match {
          case None => ParsedAst.TypeParams.Elided
          case Some(xs) => ParsedAst.TypeParams.Explicit(xs.toList)
        })
      }
    }

  }

  def Attribute: Rule1[ParsedAst.Attribute] = rule {
    SP ~ Names.Attribute ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Attribute
  }

  def TypeAndEffect: Rule2[ParsedAst.Type, Option[ParsedAst.Type]] = rule {
    Type ~ optional(WS ~ atomic("&") ~ WS ~ Type)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Uses                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  def Use: Rule1[ParsedAst.Use] = rule {
    keyword("use") ~ WS ~ (Uses.UseClass | Uses.UseOneTag | Uses.UseManyTag | Uses.UseOne | Uses.UseMany)
  }

  object Uses {
    def UseOne: Rule1[ParsedAst.Use.UseOne] = rule {
      SP ~ Names.Namespace ~ "." ~ UseName ~ SP ~> ParsedAst.Use.UseOne
    }

    def UseMany: Rule1[ParsedAst.Use.UseMany] = {
      def NameAndAlias: Rule1[ParsedAst.Use.NameAndAlias] = rule {
        SP ~ UseName ~ optional(WS ~ atomic("=>") ~ WS ~ UseName) ~ SP ~> ParsedAst.Use.NameAndAlias
      }

      rule {
        SP ~ Names.Namespace ~ "." ~ "{" ~ zeroOrMore(NameAndAlias).separatedBy(optWS ~ "," ~ optWS) ~ "}" ~ SP ~> ParsedAst.Use.UseMany
      }
    }

    def UseOneTag: Rule1[ParsedAst.Use.UseOneTag] = rule {
      SP ~ Names.QualifiedType ~ "." ~ Names.Tag ~ SP ~> ParsedAst.Use.UseOneTag
    }

    def UseManyTag: Rule1[ParsedAst.Use.UseManyTag] = {
      def TagAndAlias: Rule1[ParsedAst.Use.NameAndAlias] = rule {
        SP ~ Names.Tag ~ optional(WS ~ atomic("=>") ~ WS ~ Names.Tag) ~ SP ~> ParsedAst.Use.NameAndAlias
      }

      rule {
        SP ~ Names.QualifiedType ~ "." ~ "{" ~ zeroOrMore(TagAndAlias).separatedBy(optWS ~ "," ~ optWS) ~ "}" ~ SP ~> ParsedAst.Use.UseManyTag
      }
    }

    def UseClass: Rule1[ParsedAst.Use] = rule { // MATT improve syntax
      keyword("class") ~ WS ~ SP ~ Names.Namespace ~ "." ~ UseName ~ SP ~> ParsedAst.Use.UseClass
    }

    def UseName: Rule1[Name.Ident] = rule {
      Names.LowerCaseName | Names.UpperCaseName | Names.GreekName | Names.MathName | Names.OperatorName
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    Literals.Null | Literals.Bool | Literals.Char | Literals.Str | Literals.Default | Literals.Float | Literals.Int
  }

  object Literals {

    def Null: Rule1[ParsedAst.Literal] = rule {
      SP ~ keyword("null") ~ SP ~> ParsedAst.Literal.Null
    }

    def Bool: Rule1[ParsedAst.Literal] = {

      def True: Rule1[ParsedAst.Literal.True] = rule {
        SP ~ keyword("true") ~ SP ~> ParsedAst.Literal.True
      }

      def False: Rule1[ParsedAst.Literal.False] = rule {
        SP ~ keyword("false") ~ SP ~> ParsedAst.Literal.False
      }

      rule {
        True | False
      }
    }

    def Char: Rule1[ParsedAst.Literal.Char] = {
      def Normal: Rule1[String] = {
        def Quote: Rule0 = rule("'")

        def Backslash: Rule0 = rule("\\")

        rule {
          capture(!Quote ~ !Backslash ~ CharPredicate.All)
        }
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
      SP ~ Sign ~ SeparableDecDigits ~ "." ~ SeparableDecDigits ~ SP ~> ParsedAst.Literal.Float64
    }

    def Float32: Rule1[ParsedAst.Literal.Float32] = rule {
      SP ~ Sign ~ SeparableDecDigits ~ "." ~ SeparableDecDigits ~ atomic("f32") ~ SP ~> ParsedAst.Literal.Float32
    }

    def Float64: Rule1[ParsedAst.Literal.Float64] = rule {
      SP ~ Sign ~ SeparableDecDigits ~ "." ~ SeparableDecDigits ~ atomic("f64") ~ SP ~> ParsedAst.Literal.Float64
    }

    def Int: Rule1[ParsedAst.Literal] = rule {
      Int8 | Int16 | Int32 | Int64 | BigInt | IntDefault
    }

    def IntDefault: Rule1[ParsedAst.Literal.Int32] = rule {
      SP ~ Sign ~ RadixedInt ~ SP ~> ParsedAst.Literal.Int32
    }

    def Int8: Rule1[ParsedAst.Literal.Int8] = rule {
      SP ~ Sign ~ RadixedInt ~ atomic("i8") ~ SP ~> ParsedAst.Literal.Int8
    }

    def Int16: Rule1[ParsedAst.Literal.Int16] = rule {
      SP ~ Sign ~ RadixedInt ~ atomic("i16") ~ SP ~> ParsedAst.Literal.Int16
    }

    def Int32: Rule1[ParsedAst.Literal.Int32] = rule {
      SP ~ Sign ~ RadixedInt ~ atomic("i32") ~ SP ~> ParsedAst.Literal.Int32
    }

    def Int64: Rule1[ParsedAst.Literal.Int64] = rule {
      SP ~ Sign ~ RadixedInt ~ atomic("i64") ~ SP ~> ParsedAst.Literal.Int64
    }

    def BigInt: Rule1[ParsedAst.Literal.BigInt] = rule {
      SP ~ Sign ~ RadixedInt ~ atomic("ii") ~ SP ~> ParsedAst.Literal.BigInt
    }

    def Str: Rule1[ParsedAst.Literal.Str] = {
      def Quote: Rule0 = rule("\"")

      rule {
        SP ~ "\"" ~ capture(zeroOrMore(!Quote ~ CharPredicate.All)) ~ "\"" ~ SP ~> ParsedAst.Literal.Str
      }
    }

    def Default: Rule1[ParsedAst.Literal.Default] = rule {
      SP ~ keyword("default") ~ SP ~> ParsedAst.Literal.Default
    }

    def Sign: Rule1[Boolean] = rule {
      optional(capture("-")) ~> ((s: Option[String]) => s.nonEmpty)
    }

    def SeparableDecDigits: Rule1[String] = rule {
      capture(CharPredicate.Digit ~ zeroOrMore(zeroOrMore("_") ~ CharPredicate.Digit))
    }

    def SeparableHexDigits: Rule1[String] = rule {
      capture(CharPredicate.HexDigit ~ zeroOrMore(zeroOrMore("_") ~ CharPredicate.HexDigit))
    }

    def RadixedInt: Rule2[Int, String] = rule {
      HexInt | DecInt
    }

    def HexInt: Rule2[Int, String] = rule {
      atomic("0x") ~ push(16) ~ SeparableHexDigits
    }

    def DecInt: Rule2[Int, String] = rule {
      push(10) ~ SeparableDecDigits
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  def Expression: Rule1[ParsedAst.Expression] = rule {
    Expressions.Assign
  }

  object Expressions {
    def Stm: Rule1[ParsedAst.Expression] = rule {
      Expression ~ optional(optWS ~ atomic(";") ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.Stm)
    }

    def Assign: Rule1[ParsedAst.Expression] = rule {
      PutChannel ~ optional(optWS ~ atomic(":=") ~ optWS ~ PutChannel ~ SP ~> ParsedAst.Expression.Assign)
    }

    def PutChannel: Rule1[ParsedAst.Expression] = rule {
      LogicalOr ~ zeroOrMore(optWS ~ atomic("<-") ~ optWS ~ LogicalOr ~ SP ~> ParsedAst.Expression.PutChannel)
    }

    def LogicalOr: Rule1[ParsedAst.Expression] = {
      def Or1: Rule1[String] = rule {
        optWS ~ capture(atomic("||")) ~ optWS
      }
      def Or2: Rule1[String] = rule {
        WS ~ capture(keyword("or")) ~ WS
      }
      rule {
        LogicalAnd ~ zeroOrMore((Or1 | Or2) ~ LogicalAnd ~ SP ~> ParsedAst.Expression.Binary)
      }
    }

    def LogicalAnd: Rule1[ParsedAst.Expression] = {
      def And1: Rule1[String] = rule {
        optWS ~ capture(atomic("&&")) ~ optWS
      }
      def And2: Rule1[String] = rule {
        WS ~ capture(keyword("and")) ~ WS
      }
      rule {
        BitwiseOr ~ zeroOrMore((And1 | And2) ~ BitwiseOr ~ SP ~> ParsedAst.Expression.Binary)
      }
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
      Relational ~ optional(optWS ~ capture(atomic("==") | atomic("!=") | atomic("<=>")) ~ optWS ~ Relational ~ SP ~> ParsedAst.Expression.Binary)
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
        capture("**" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "=>" | "->" | "<-" | "|=" | "or")
      }

      // NB: We allow any operator, other than a reserved operator, to be matched by this rule.
      def Reserved3: Rule1[String] = rule {
        capture("<<<" | ">>>" | "<+>" | "not" | "and")
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

    def Unary: Rule1[ParsedAst.Expression] = {
      def UnaryOp1: Rule1[String] = rule {
        capture(atomic("!") | atomic("+") | atomic("-") | atomic("~~~"))
      }
      def UnaryOp2: Rule1[String] = rule {
        capture(keyword("not")) ~ WS
      }
      rule {
        !Literal ~ (SP ~ (UnaryOp1 | UnaryOp2) ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Unary) | Ref
      }
    }

    def Ref: Rule1[ParsedAst.Expression] = rule {
      (SP ~ keyword("ref") ~ WS ~ Ref ~ SP ~> ParsedAst.Expression.Ref) | Deref
    }

    def Deref: Rule1[ParsedAst.Expression] = rule {
      (SP ~ keyword("deref") ~ WS ~ Deref ~ SP ~> ParsedAst.Expression.Deref) | Cast
    }

    def Cast: Rule1[ParsedAst.Expression] = rule {
      Ascribe ~ optional(WS ~ keyword("as") ~ WS ~ TypAndEffFragment ~ SP ~> ParsedAst.Expression.Cast)
    }

    def Ascribe: Rule1[ParsedAst.Expression] = rule {
      FAppend ~ optional(optWS ~ ":" ~ optWS ~ TypAndEffFragment ~ SP ~> ParsedAst.Expression.Ascribe)
    }

    def TypAndEffFragment: Rule2[Option[ParsedAst.Type], Option[ParsedAst.Type]] = {
      def SomeTyp: Rule1[Option[ParsedAst.Type]] = rule {
        Type ~> ((tpe: ParsedAst.Type) => Some(tpe))
      }

      def SomeEff: Rule1[Option[ParsedAst.Type]] = rule {
        Type ~> ((tpe: ParsedAst.Type) => Some(tpe))
      }

      def TypOnly: Rule2[Option[ParsedAst.Type], Option[ParsedAst.Type]] = rule {
        SomeTyp ~ push(None)
      }

      def EffOnly: Rule2[Option[ParsedAst.Type], Option[ParsedAst.Type]] = rule {
        push(None) ~ atomic("&") ~ WS ~ SomeEff
      }

      def TypAndEff: Rule2[Option[ParsedAst.Type], Option[ParsedAst.Type]] = rule {
        SomeTyp ~ WS ~ atomic("&") ~ WS ~ SomeEff
      }

      rule {
        TypAndEff | TypOnly | EffOnly
      }
    }

    def Primary: Rule1[ParsedAst.Expression] = rule {
      LetMatch | LetMatchStar | LetUse | LetImport | IfThenElse | NullMatch | Match | LambdaMatch | TryCatch | Lambda | Tuple |
        RecordOperation | RecordLiteral | Block | RecordSelectLambda | NewChannel |
        GetChannel | SelectChannel | Spawn | Lazy | Force | ArrayLit | ArrayNew |
        FNil | FSet | FMap | ConstraintSet | FixpointSolve | FixpointFold |
        FixpointProject | Constraint | Interpolation | Literal | Existential | Universal |
        UnaryLambda | QName | Tag | SName | Hole
    }

    def Literal: Rule1[ParsedAst.Expression.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Expression.Lit
    }

    def Interpolation: Rule1[ParsedAst.Expression.Interpolation] = {
      def DblQuote: Rule0 = rule("\"")

      def DollarLBrace: Rule0 = rule("${")

      def RBrace: Rule0 = rule("}")

      def ExpPart: Rule1[ParsedAst.InterpolationPart] = rule {
        DollarLBrace ~ optWS ~ Expression ~ optWS ~ RBrace ~> ParsedAst.InterpolationPart.ExpPart
      }

      def StrPart: Rule1[ParsedAst.InterpolationPart] = rule {
        capture(oneOrMore(!(DblQuote | DollarLBrace) ~ CharPredicate.All)) ~> ParsedAst.InterpolationPart.StrPart
      }

      def InterpolationPart: Rule1[ParsedAst.InterpolationPart] = rule {
        ExpPart | StrPart
      }

      rule {
        SP ~ DblQuote ~ zeroOrMore(InterpolationPart) ~ DblQuote ~ SP ~> ParsedAst.Expression.Interpolation
      }
    }

    def IfThenElse: Rule1[ParsedAst.Expression.IfThenElse] = rule {
      SP ~ keyword("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ WS ~ keyword("else") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
    }

    def LetMatch: Rule1[ParsedAst.Expression.LetMatch] = rule {
      SP ~ keyword("let") ~ WS ~ Pattern ~ optWS ~ optional(":" ~ optWS ~ Type ~ optWS) ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetMatch
    }

    def LetMatchStar: Rule1[ParsedAst.Expression.LetMatchStar] = rule {
      SP ~ keyword("let*") ~ WS ~ Pattern ~ optWS ~ optional(":" ~ optWS ~ Type ~ optWS) ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetMatchStar
    }

    def LetUse: Rule1[ParsedAst.Expression.Use] = rule {
      SP ~ Use ~ optWS ~ ";" ~ optWS ~ Expressions.Stm ~ SP ~> ParsedAst.Expression.Use
    }

    def LetImport: Rule1[ParsedAst.Expression] = {

      def JvmIdent: Rule1[String] = rule {
        capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum + '_'))
      }

      def JvmName: Rule1[Seq[String]] = rule {
        oneOrMore(JvmIdent).separatedBy(".")
      }

      def JvmStaticName: Rule1[Seq[String]] = rule {
        oneOrMore(JvmIdent).separatedBy(".") ~ ":" ~ JvmIdent ~> ((xs: Seq[String], x: String) => xs :+ x)
      }

      def Constructor: Rule1[ParsedAst.JvmOp] = rule {
        keyword("new") ~ WS ~ JvmName ~ optWS ~ Signature ~ WS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.Constructor
      }

      def Method: Rule1[ParsedAst.JvmOp] = rule {
        JvmName ~ optWS ~ Signature ~ optional(WS ~ keyword("as") ~ WS ~ Names.Variable) ~> ParsedAst.JvmOp.Method
      }

      def StaticMethod: Rule1[ParsedAst.JvmOp] = rule {
        JvmStaticName ~ optWS ~ Signature ~ optional(WS ~ keyword("as") ~ WS ~ Names.Variable) ~> ParsedAst.JvmOp.StaticMethod
      }

      def GetField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("get") ~ WS ~ JvmName ~ WS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.GetField
      }

      def PutField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("set") ~ WS ~ JvmName ~ WS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.PutField
      }

      def GetStaticField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("get") ~ WS ~ JvmStaticName ~ WS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.GetStaticField
      }

      def PutStaticField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("set") ~ WS ~ JvmStaticName ~ WS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.PutStaticField
      }

      def Signature: Rule1[Seq[ParsedAst.Type]] = rule {
        "(" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
      }

      def Import: Rule1[ParsedAst.JvmOp] = rule {
        keyword("import") ~ WS ~ (Constructor | Method | StaticMethod | GetField | PutField | GetStaticField | PutStaticField)
      }

      rule {
        SP ~ Import ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetImport
      }
    }

    def Match: Rule1[ParsedAst.Expression.Match] = {
      def Rule: Rule1[ParsedAst.MatchRule] = rule {
        keyword("case") ~ WS ~ Pattern ~ optWS ~ optional(keyword("if") ~ WS ~ Expression ~ optWS) ~ atomic("=>") ~ optWS ~ Stm ~> ParsedAst.MatchRule
      }

      rule {
        SP ~ keyword("match") ~ WS ~ Expression ~ optWS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(CaseSeparator) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Match
      }
    }

    def NullMatch: Rule1[ParsedAst.Expression.NullMatch] = {
      def MatchOne: Rule1[Seq[ParsedAst.Expression]] = rule {
        Expression ~> ((e: ParsedAst.Expression) => Seq(e))
      }

      def MatchMany: Rule1[Seq[ParsedAst.Expression]] = rule {
        "(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
      }

      def ChoicePattern: Rule1[ParsedAst.ChoicePattern] = rule {
        (SP ~ atomic("_") ~ SP ~> ParsedAst.ChoicePattern.Wild) |
          (SP ~ keyword("null") ~ SP ~> ParsedAst.ChoicePattern.Absent) |
          (SP ~ Names.Variable ~ SP ~> ParsedAst.ChoicePattern.Present)
      }

      def CaseOne: Rule1[ParsedAst.ChoiceRule] = rule {
        SP ~ keyword("case") ~ WS ~ ChoicePattern ~ WS ~ atomic("=>") ~ WS ~ Expression ~ SP ~>
          ((sp1: SourcePosition, x: ParsedAst.ChoicePattern, e: ParsedAst.Expression, sp2: SourcePosition) => ParsedAst.ChoiceRule(sp1, Seq(x), e, sp2))
      }

      def CaseMany: Rule1[ParsedAst.ChoiceRule] = rule {
        SP ~ keyword("case") ~ WS ~ "(" ~ optWS ~ oneOrMore(ChoicePattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ WS ~ atomic("=>") ~ WS ~ Expression ~ SP ~> ParsedAst.ChoiceRule
      }

      rule {
        SP ~ keyword("match?") ~ WS ~ (MatchMany | MatchOne) ~ optWS ~ "{" ~ optWS ~ oneOrMore(CaseMany | CaseOne).separatedBy(WS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.NullMatch
      }
    }

    def TryCatch: Rule1[ParsedAst.Expression] = {
      def CatchRule: Rule1[ParsedAst.CatchRule] = rule {
        keyword("case") ~ WS ~ Names.Variable ~ optWS ~ ":" ~ optWS ~ atomic("##") ~ Names.JavaName ~ WS ~ atomic("=>") ~ optWS ~ Expression ~> ParsedAst.CatchRule
      }

      def CatchBody: Rule1[Seq[ParsedAst.CatchRule]] = rule {
        "{" ~ optWS ~ oneOrMore(CatchRule).separatedBy(CaseSeparator) ~ optWS ~ "}"
      }

      rule {
        SP ~ keyword("try") ~ WS ~ Expression ~ optWS ~ keyword("catch") ~ optWS ~ CatchBody ~ SP ~> ParsedAst.Expression.TryCatch
      }
    }

    def RecordSelect: Rule1[ParsedAst.Expression] = rule {
      Postfix ~ zeroOrMore(optWS ~ "." ~ Names.Field ~ SP ~> ParsedAst.Expression.RecordSelect)
    }

    //TODO SJ: order this with primaries
    def NewChannel: Rule1[ParsedAst.Expression.NewChannel] = rule {
      SP ~ keyword("chan") ~ WS ~ Type ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.NewChannel
    }

    def GetChannel: Rule1[ParsedAst.Expression.GetChannel] = rule {
      SP ~ atomic("<-") ~ WS ~ RecordSelect ~ SP ~> ParsedAst.Expression.GetChannel
    }

    def SelectChannel: Rule1[ParsedAst.Expression.SelectChannel] = {
      def SelectChannelRule: Rule1[ParsedAst.SelectChannelRule] = rule {
        keyword("case") ~ WS ~ Names.Variable ~ optWS ~ atomic("<-") ~ optWS ~ Expression ~ optWS ~ atomic("=>") ~ optWS ~ Stm ~> ParsedAst.SelectChannelRule
      }

      def SelectChannelDefault: Rule1[ParsedAst.Expression] = rule {
        keyword("case") ~ WS ~ atomic("_") ~ optWS ~ atomic("=>") ~ optWS ~ Stm
      }

      rule {
        SP ~ keyword("select") ~ WS ~ "{" ~ optWS ~ oneOrMore(SelectChannelRule).separatedBy(CaseSeparator) ~ optWS ~ optional(SelectChannelDefault) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.SelectChannel
      }
    }

    def Spawn: Rule1[ParsedAst.Expression.Spawn] = rule {
      SP ~ keyword("spawn") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Spawn
    }

    def Lazy: Rule1[ParsedAst.Expression.Lazy] = rule {
      SP ~ keyword("lazy") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Lazy
    }

    def Force: Rule1[ParsedAst.Expression.Force] = rule {
      SP ~ keyword("force") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Force
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
      Nullify ~ optional(oneOrMore(optWS ~ "[" ~ optWS ~ Expression ~ optWS ~ "]") ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.ArrayStore)
    }

    def Nullify: Rule1[ParsedAst.Expression] = rule {
      Apply ~ optional("?" ~ SP ~> ParsedAst.Expression.Nullify)
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
      "{" ~ optWS ~ Stm ~ optWS ~ "}"
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

    def FNil: Rule1[ParsedAst.Expression.FNil] = rule {
      SP ~ keyword("Nil") ~ SP ~> ParsedAst.Expression.FNil
    }

    def FAppend: Rule1[ParsedAst.Expression] = rule {
      FList ~ optional(optWS ~ SP ~ atomic(":::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FAppend)
    }

    def FList: Rule1[ParsedAst.Expression] = rule {
      RecordSelect ~ optional(optWS ~ SP ~ atomic("::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FCons)
    }

    def FSet: Rule1[ParsedAst.Expression.FSet] = rule {
      SP ~ "Set#{" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.FSet
    }

    def FMap: Rule1[ParsedAst.Expression.FMap] = {
      def KeyValue: Rule1[(ParsedAst.Expression, ParsedAst.Expression)] = rule {
        Expression ~ optWS ~ atomic("->") ~ optWS ~ Expression ~> ((e1: ParsedAst.Expression, e2: ParsedAst.Expression) => (e1, e2))
      }

      rule {
        SP ~ "Map#{" ~ optWS ~ zeroOrMore(KeyValue).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.FMap
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
      SP ~ keyword("match") ~ optWS ~ Pattern ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.LambdaMatch
    }

    def Constraint: Rule1[ParsedAst.Expression] = rule {
      SP ~ Declarations.Constraint ~ SP ~> ParsedAst.Expression.FixpointConstraint
    }

    def ConstraintSet: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("#{") ~ optWS ~ zeroOrMore(Declarations.Constraint) ~ optWS ~ atomic("}") ~ SP ~> ParsedAst.Expression.FixpointConstraintSet
    }

    def FixpointSolve: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("solve") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.FixpointSolve
    }

    def FixpointProject: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("project") ~ WS ~ Names.Predicate ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.FixpointProject
    }

    def FixpointFold: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("fold") ~ WS ~ Names.Predicate ~ WS ~ Expression ~ WS ~ Expression ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.FixpointFold
    }

    // TODO: We should only allow one variant of these.
    def Existential: Rule1[ParsedAst.Expression.Existential] = rule {
      SP ~ ("∃" | keyword("exists")) ~ optWS ~ Declarations.TypeParams ~ optWS ~ FormalParamList ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Existential
    }

    // TODO: We should only allow one variant of these.
    def Universal: Rule1[ParsedAst.Expression.Universal] = rule {
      SP ~ ("∀" | keyword("forall")) ~ optWS ~ Declarations.TypeParams ~ optWS ~ FormalParamList ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Universal
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
      FNil | Tag | Lit | Tuple | Array | ArrayTailSpread | ArrayHeadSpread | Var
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

    def Array: Rule1[ParsedAst.Pattern.Array] = rule {
      SP ~ "[" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.Pattern.Array
    }

    def ArrayTailSpread: Rule1[ParsedAst.Pattern.ArrayTailSpread] = rule {
      SP ~ "[" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "," ~ optWS ~ ".." ~ Names.Variable ~ optWS ~ "]" ~ SP ~> ParsedAst.Pattern.ArrayTailSpread
    }

    def ArrayHeadSpread: Rule1[ParsedAst.Pattern.ArrayHeadSpread] = rule {
      SP ~ "[" ~ optWS ~ Names.Variable ~ ".." ~ optWS ~ "," ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.Pattern.ArrayHeadSpread
    }

    def FNil: Rule1[ParsedAst.Pattern.FNil] = rule {
      SP ~ keyword("Nil") ~ SP ~> ParsedAst.Pattern.FNil
    }

    def FList: Rule1[ParsedAst.Pattern] = rule {
      Simple ~ optional(optWS ~ SP ~ atomic("::") ~ SP ~ optWS ~ Pattern ~> ParsedAst.Pattern.FCons)
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Predicates                                                              //
  /////////////////////////////////////////////////////////////////////////////
  def HeadPredicate: Rule1[ParsedAst.Predicate.Head] = rule {
    Predicates.Head.Atom | Predicates.Head.Union
  }

  def BodyPredicate: Rule1[ParsedAst.Predicate.Body] = rule {
    Predicates.Body.Positive | Predicates.Body.Negative | Predicates.Body.Guard | Predicates.Body.Filter
  }

  object Predicates {

    object Head {

      def Atom: Rule1[ParsedAst.Predicate.Head.Atom] = rule {
        SP ~ Names.Predicate ~ optWS ~ Predicates.TermList ~ SP ~> ParsedAst.Predicate.Head.Atom
      }

      def Union: Rule1[ParsedAst.Predicate.Head.Union] = rule {
        SP ~ keyword("union") ~ WS ~ Expression ~ SP ~> ParsedAst.Predicate.Head.Union
      }

    }

    object Body {

      def Positive: Rule1[ParsedAst.Predicate.Body.Atom] = rule {
        SP ~ push(Polarity.Positive) ~ Names.Predicate ~ optWS ~ Predicates.PatternList ~ SP ~> ParsedAst.Predicate.Body.Atom
      }

      def Negative: Rule1[ParsedAst.Predicate.Body.Atom] = {
        def Not: Rule0 = rule {
          "!" | (keyword("not") ~ WS)
        }

        rule {
          SP ~ push(Polarity.Negative) ~ Not ~ optWS ~ Names.Predicate ~ optWS ~ Predicates.PatternList ~ SP ~> ParsedAst.Predicate.Body.Atom
        }
      }

      def Guard: Rule1[ParsedAst.Predicate.Body.Guard] = rule {
        SP ~ keyword("if") ~ WS ~ Expression ~ SP ~> ParsedAst.Predicate.Body.Guard
      }

      def Filter: Rule1[ParsedAst.Predicate.Body.Filter] = rule {
        SP ~ Names.QualifiedDefinition ~ optWS ~ ArgumentList ~ SP ~> ParsedAst.Predicate.Body.Filter
      }
    }

    def TermList: Rule2[Seq[ParsedAst.Expression], Option[ParsedAst.Expression]] = rule {
      "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optional(optWS ~ ";" ~ optWS ~ Expression) ~ optWS ~ ")"
    }

    def PatternList: Rule2[Seq[ParsedAst.Pattern], Option[ParsedAst.Pattern]] = rule {
      "(" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optional(optWS ~ ";" ~ optWS ~ Pattern) ~ optWS ~ ")"
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
      Or ~ optional(
        (optWS ~ atomic("~>") ~ optWS ~ Type ~ SP ~> ParsedAst.Type.UnaryImpureArrow) |
          (optWS ~ atomic("->") ~ optWS ~ Type ~ optional(WS ~ atomic("&") ~ WS ~ Type) ~ SP ~> ParsedAst.Type.UnaryPolymorphicArrow)
      )
    }

    def Or: Rule1[ParsedAst.Type] = rule {
      And ~ zeroOrMore(WS ~ keyword("or") ~ WS ~ Type ~> ParsedAst.Type.Or)
    }

    def And: Rule1[ParsedAst.Type] = rule {
      Nullable ~ zeroOrMore(WS ~ keyword("and") ~ WS ~ Type ~> ParsedAst.Type.And)
    }

    def Nullable: Rule1[ParsedAst.Type] = rule {
      Apply ~ optional(optWS ~ atomic("?") ~ optional(optWS ~ Or) ~ SP ~> ParsedAst.Type.Nullable)
    }

    def Apply: Rule1[ParsedAst.Type] = rule {
      Primary ~ zeroOrMore(TypeArguments ~ SP ~> ParsedAst.Type.Apply)
    }

    def Primary: Rule1[ParsedAst.Type] = rule {
      Arrow | Tuple | Record | Schema | Native | True | False | Pure | Impure | Not | Var | Ambiguous
    }

    def Arrow: Rule1[ParsedAst.Type] = {
      def TypeList: Rule1[Seq[ParsedAst.Type]] = rule {
        "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
      }

      rule {
        SP ~ TypeList ~ optWS ~ (
          (atomic("~>") ~ optWS ~ Type ~ SP ~> ParsedAst.Type.ImpureArrow) |
            (atomic("->") ~ optWS ~ Type ~ optional(WS ~ atomic("&") ~ WS ~ Type) ~ SP ~> ParsedAst.Type.PolymorphicArrow)
          )
      }
    }

    def Tuple: Rule1[ParsedAst.Type] = {
      def Unit: Rule1[ParsedAst.Type] = rule {
        SP ~ atomic("()") ~ SP ~> ParsedAst.Type.Unit
      }

      def Singleton: Rule1[ParsedAst.Type] = rule {
        "(" ~ optWS ~ Type ~ optWS ~ ")"
      }

      def Tuple: Rule1[ParsedAst.Type] = rule {
        SP ~ "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Type.Tuple
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

    def Schema: Rule1[ParsedAst.Type] = {
      def PredicateWithAlias: Rule1[ParsedAst.PredicateType.PredicateWithAlias] = rule {
        SP ~ Names.QualifiedPredicate ~ optional(TypeArguments) ~ SP ~> ParsedAst.PredicateType.PredicateWithAlias
      }

      def RelPredicateWithTypes: Rule1[ParsedAst.PredicateType.RelPredicateWithTypes] = rule {
        SP ~ Names.Predicate ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.PredicateType.RelPredicateWithTypes
      }

      def LatPredicateWithTypes: Rule1[ParsedAst.PredicateType.LatPredicateWithTypes] = rule {
        SP ~ Names.Predicate ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ";" ~ optWS ~ Type ~ optWS ~ ")" ~ SP ~> ParsedAst.PredicateType.LatPredicateWithTypes
      }

      rule {
        SP ~ atomic("#{") ~ optWS ~ zeroOrMore(RelPredicateWithTypes | LatPredicateWithTypes | PredicateWithAlias).separatedBy(optWS ~ "," ~ optWS) ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ "}" ~ SP ~> ParsedAst.Type.Schema
      }
    }

    def Native: Rule1[ParsedAst.Type] = rule {
      SP ~ atomic("##") ~ Names.JavaName ~ SP ~> ParsedAst.Type.Native
    }

    def True: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("true") ~ SP ~> ParsedAst.Type.True
    }

    def False: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("false") ~ SP ~> ParsedAst.Type.False
    }

    def Pure: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("Pure") ~ SP ~> ParsedAst.Type.True
    }

    def Impure: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("Impure") ~ SP ~> ParsedAst.Type.False
    }

    def Not: Rule1[ParsedAst.Type] = rule {
      // NB: We must not use Type here because it gives the wrong precedence.
      keyword("not") ~ WS ~ Nullable ~> ParsedAst.Type.Not
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
  // Helpers                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  def FormalParam: Rule1[ParsedAst.FormalParam] = rule {
    // TODO: A quick hack to allow mut annotations.
    SP ~ Modifiers ~ Names.Variable ~ optional(optWS ~ ":" ~ optWS ~ optional(keyword("mut") ~ WS) ~ Type) ~ SP ~> ParsedAst.FormalParam
  }

  def FormalParamList: Rule1[Seq[ParsedAst.FormalParam]] = rule {
    "(" ~ optWS ~ zeroOrMore(FormalParam).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def ArgumentList: Rule1[Seq[ParsedAst.Expression]] = rule {
    "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def OptArgumentList: Rule1[Option[Seq[ParsedAst.Expression]]] = rule {
    optional(ArgumentList)
  }

  def AttributeList: Rule1[Seq[ParsedAst.Attribute]] = rule {
    "(" ~ optWS ~ zeroOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def Annotations: Rule1[Seq[ParsedAst.AnnotationOrProperty]] = rule {
    zeroOrMore(Annotation | Property).separatedBy(WS) ~ optWS
  }

  def Modifiers: Rule1[Seq[ParsedAst.Modifier]] = {
    def Inline: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("inline")) ~ SP ~> ParsedAst.Modifier
    }

    def Public: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("pub")) ~ SP ~> ParsedAst.Modifier
    }

    def Modifier: Rule1[ParsedAst.Modifier] = rule {
      Inline | Public
    }

    rule {
      zeroOrMore(Modifier).separatedBy(WS) ~ optWS
    }
  }

  def Annotation: Rule1[ParsedAst.AnnotationOrProperty] = rule {
    SP ~ atomic("@") ~ Names.Annotation ~ OptArgumentList ~ SP ~> ParsedAst.Annotation
  }

  def Property: Rule1[ParsedAst.AnnotationOrProperty] = rule {
    SP ~ atomic("#") ~ Names.QualifiedDefinition ~ OptArgumentList ~ SP ~> ParsedAst.Property
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

    def Annotation: Rule1[Name.Ident] = rule {
      LowerCaseName | UpperCaseName
    }

    def Attribute: Rule1[Name.Ident] = LowerCaseName

    def Class: Rule1[Name.Ident] = UpperCaseName

    def QualifiedClass: Rule1[Name.QName] = UpperCaseQName

    def Definition: Rule1[Name.Ident] = rule {
      LowerCaseName | GreekName | MathName | OperatorName
    }

    def QualifiedDefinition: Rule1[Name.QName] = LowerCaseQName

    def Eff: Rule1[Name.Ident] = LowerCaseName

    def Field: Rule1[Name.Ident] = LowerCaseName

    def Hole: Rule1[Name.Ident] = LowerCaseName

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
  // Keyword                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Reads the keyword and looks ahead to ensure there no legal letters immediately following.
    */
  def keyword(word: String): Rule0 = rule {
    atomic(word) ~ !Names.LegalLetter
  }

  /////////////////////////////////////////////////////////////////////////////
  // Case Separator                                                          //
  /////////////////////////////////////////////////////////////////////////////
  def CaseSeparator: Rule0 = rule {
    (optWS ~ "," ~ optWS) | WS
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
    def MultiLineComment: Rule0 = {
      def SlashStar: Rule0 = rule("/*")

      def StarSlash: Rule0 = rule("*/")

      rule {
        SlashStar ~ zeroOrMore(!StarSlash ~ ANY) ~ StarSlash
      }
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
