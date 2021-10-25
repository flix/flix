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
import ca.uwaterloo.flix.language.CompilationMessage
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
  def run(sources: List[Source])(implicit flix: Flix): Validation[ParsedAst.Program, CompilationMessage] = flix.phase("Parser") {
    // Parse each source in parallel.
    val roots = sequence(ParOps.parMap(sources, parseRoot))

    // Sequence and combine the ASTs into one abstract syntax tree.
    mapN(roots) {
      case as => ParsedAst.Program(as)
    }
  }

  private def reduceLiteralWhitespaceChars(s: String): String =
    s.replaceAll("\\\\n|\\\\r|\\\\t"," ")

  /**
    * Attempts to parse the given `source` as a root.
    */
  def parseRoot(source: Source)(implicit flix: Flix): Validation[ParsedAst.Root, CompilationMessage] = {
    flix.subtask(source.name)

    val parser = new Parser(source)
    parser.Root.run() match {
      case scala.util.Success(ast) =>
        ast.toSuccess
      case scala.util.Failure(e: org.parboiled2.ParseError) =>
        val loc = SourceLocation(source, e.position.line, e.position.column, e.position.line, e.position.column, _ => "")
        ca.uwaterloo.flix.language.errors.ParseError(reduceLiteralWhitespaceChars(parser.formatError(e)), loc).toFailure
      case scala.util.Failure(e) =>
        ca.uwaterloo.flix.language.errors.ParseError(e.getMessage, SourceLocation.Unknown).toFailure
    }
  }

  /**
    * Attempts to parse the given `source` as an expression.
    */
  def parseExp(source: Source): Validation[ParsedAst.Expression, CompilationMessage] = {
    val parser = new Parser(source)
    parser.Expression.run() match {
      case scala.util.Success(ast) =>
        ast.toSuccess
      case scala.util.Failure(e: org.parboiled2.ParseError) =>
        val loc = SourceLocation(source, e.position.line, e.position.column, e.position.line, e.position.column, _ => "")
        ca.uwaterloo.flix.language.errors.ParseError(reduceLiteralWhitespaceChars(parser.formatError(e)), loc).toFailure
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
    def Uses: Rule1[Seq[ParsedAst.Use]] = namedRule("UseDeclaration") {
      zeroOrMore(Use ~ optWS ~ ";").separatedBy(optWS)
    }

    def Decls: Rule1[Seq[ParsedAst.Declaration]] = namedRule("Declaration") {
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
      Declarations.Def |
      Declarations.Law |
      Declarations.Enum |
      Declarations.OpaqueType |
      Declarations.TypeAlias |
      Declarations.Relation |
      Declarations.Lattice |
      Declarations.Class |
      Declarations.Instance
  }

  object Declarations {

    def Namespace: Rule1[ParsedAst.Declaration.Namespace] = {
      def Uses: Rule1[Seq[ParsedAst.Use]] = namedRule("UseDeclaration") {
        zeroOrMore(Use ~ optWS ~ ";").separatedBy(optWS)
      }

      def Decls: Rule1[Seq[ParsedAst.Declaration]] = namedRule("Declaration") {
        zeroOrMore(Declaration)
      }

      rule {
        optWS ~ SP ~ keyword("namespace") ~ WS ~ Names.Namespace ~ optWS ~ '{' ~ optWS ~ Uses ~ Decls ~ optWS ~ '}' ~ SP ~> ParsedAst.Declaration.Namespace
      }
    }

    def Def: Rule1[ParsedAst.Declaration.Def] = namedRule("Def") {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ OptTypeConstraintList ~ optWS ~ "=" ~ optWS ~ Expressions.Stm ~ SP ~> ParsedAst.Declaration.Def
    }

    def Sig: Rule1[ParsedAst.Declaration.Sig] = namedRule("Def") {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ OptTypeConstraintList ~ optional(optWS ~ "=" ~ optWS ~ Expressions.Stm) ~ SP ~> ParsedAst.Declaration.Sig
    }

    def Law: Rule1[ParsedAst.Declaration.Law] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("law") ~ WS ~ Names.Definition ~ optWS ~ ":" ~ optWS ~ keyword("forall") ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ OptTypeConstraintList ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Declaration.Law
    }

    def Enum: Rule1[ParsedAst.Declaration.Enum] = {
      def Case: Rule1[ParsedAst.Case] = {
        def CaseWithUnit: Rule1[ParsedAst.Case] = namedRule("Case") {
          SP ~ Names.Tag ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) =>
            ParsedAst.Case(sp1, ident, ParsedAst.Type.Unit(sp1, sp2), sp2))
        }

        def CaseWithType: Rule1[ParsedAst.Case] = namedRule("Case") {
          SP ~ Names.Tag ~ Type ~ SP ~> ParsedAst.Case
        }

        rule {
          CaseWithType | CaseWithUnit
        }
      }

      def NonEmptyCaseList: Rule1[Seq[ParsedAst.Case]] = namedRule("Case") {
        // Note: We use the case keyword as part of the separator with or without a comma.
        keyword("case") ~ WS ~ oneOrMore(Case).separatedBy(
          (optWS ~ "," ~ optWS ~ keyword("case") ~ WS) | (WS ~ keyword("case") ~ WS) | (optWS ~ "," ~ optWS)
        )
      }

      def EmptyBody = namedRule("Body") {
        push(Nil)
      }

      def NonEmptyBody = namedRule("Body") {
        optWS ~ "{" ~ optWS ~ optional(NonEmptyCaseList) ~ optWS ~ "}" ~> ((o: Option[Seq[ParsedAst.Case]]) => o.getOrElse(Seq.empty))
      }

      def Body = rule {
        NonEmptyBody | EmptyBody
      }

      rule {
        Documentation ~ Modifiers ~ SP ~ keyword("enum") ~ WS ~ Names.Type ~ TypeParams ~ Derivations ~ optWS ~ Body ~ SP ~> ParsedAst.Declaration.Enum
      }
    }

    def OpaqueType: Rule1[ParsedAst.Declaration.OpaqueType] = rule {
      Documentation ~ Modifiers ~ SP ~ keyword("opaque") ~ WS ~ keyword("type") ~ WS ~ Names.Type ~ optWS ~ TypeParams ~ Derivations ~ optWS ~ "=" ~ optWS ~ Type ~ SP ~> ParsedAst.Declaration.OpaqueType
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

    def Class: Rule1[ParsedAst.Declaration] = {
      def Head = namedRule("Class") {
        Documentation ~ Modifiers ~ SP ~ keyword("class") ~ WS ~ Names.Class ~ optWS ~ "[" ~ optWS ~ TypeParam ~ optWS ~ "]" ~ OptTypeConstraintList
      }

      def EmptyBody = namedRule("ClassBody") {
        push(Nil) ~ SP
      }

      def NonEmptyBody = namedRule("ClassBody") {
        optWS ~ "{" ~ optWS ~ zeroOrMore(Declarations.Law | Declarations.Sig).separatedBy(WS) ~ optWS ~ "}" ~ SP
      }

      rule {
        Head ~ (NonEmptyBody | EmptyBody) ~> ParsedAst.Declaration.Class
      }
    }

    def TypeConstraint: Rule1[ParsedAst.TypeConstraint] = rule {
      SP ~ Names.QualifiedClass ~ optWS ~ "[" ~ optWS ~ Type ~ optWS ~ "]" ~ SP ~> ParsedAst.TypeConstraint
    }

    def OptTypeConstraintList: Rule1[Seq[ParsedAst.TypeConstraint]] = namedRule("TypeConstraintList") {
      optional(WS ~ keyword("with") ~ WS ~ oneOrMore(TypeConstraint).separatedBy(optWS ~ "," ~ optWS)) ~> ((o: Option[Seq[ParsedAst.TypeConstraint]]) => o.getOrElse(Seq.empty))
    }

    def Instance: Rule1[ParsedAst.Declaration] = {
      def Head = namedRule("Instance") {
        Documentation ~ Modifiers ~ SP ~ keyword("instance") ~ WS ~ Names.QualifiedClass ~ optWS ~ "[" ~ optWS ~ Type ~ optWS ~ "]" ~ OptTypeConstraintList
      }

      def EmptyBody = namedRule("InstanceBody") {
        push(Nil) ~ SP
      }

      def NonEmptyBody = namedRule("InstanceBody") {
        optWS ~ "{" ~ optWS ~ zeroOrMore(Declarations.Def).separatedBy(WS) ~ optWS ~ "}" ~ SP
      }

      rule {
        Head ~ (NonEmptyBody | EmptyBody) ~> ParsedAst.Declaration.Instance
      }
    }

    def TypeParam: Rule1[ParsedAst.TypeParam] = namedRule("TypeParameter") {
      SP ~ Names.Variable ~ optional(optWS ~ ":" ~ optWS ~ Kind) ~ SP ~> ParsedAst.TypeParam
    }

    def TypeParams: Rule1[ParsedAst.TypeParams] = {
      namedRule("TypeParameterList") {
        optional("[" ~ optWS ~ oneOrMore(TypeParam).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]") ~> ((o: Option[Seq[ParsedAst.TypeParam]]) => o match {
          case None => ParsedAst.TypeParams.Elided
          case Some(xs) => ParsedAst.TypeParams.Explicit(xs.toList)
        })
      }
    }

    def Derivations: Rule1[Seq[Name.QName]] = rule {
      optWS ~ optional(keyword("with") ~ WS ~ oneOrMore(Names.QualifiedClass).separatedBy(optWS ~ "," ~ optWS)) ~> ((o: Option[Seq[Name.QName]]) => o.getOrElse(Seq.empty))
    }

  }

  def Attribute: Rule1[ParsedAst.Attribute] = rule {
    SP ~ Names.Attribute ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Attribute
  }

  def TypeAndEffect: Rule2[ParsedAst.Type, Option[ParsedAst.Type]] = namedRule("Type") {
    Type ~ optional(WS ~ "&" ~ WS ~ Type)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Uses                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  def Use: Rule1[ParsedAst.Use] = namedRule("UseDeclaration") {
    keyword("use") ~ WS ~ (Uses.UseOneTag | Uses.UseManyTag | Uses.UseOne | Uses.UseMany)
  }

  object Uses {
    def UseOne: Rule1[ParsedAst.Use.UseOne] = namedRule("SingleUse") {
      SP ~ Names.Namespace ~ "." ~ UseName ~ SP ~> ParsedAst.Use.UseOne
    }

    def UseMany: Rule1[ParsedAst.Use.UseMany] = {
      def NameAndAlias: Rule1[ParsedAst.Use.NameAndAlias] = namedRule("Identifier") {
        SP ~ UseName ~ optional(atomic((WS ~ atomic("=>") ~ WS).named("' => '")) ~ UseName) ~ SP ~> ParsedAst.Use.NameAndAlias
      }

      namedRule("MultipleUse") {
        SP ~ Names.Namespace ~ atomic(".{") ~ zeroOrMore(NameAndAlias).separatedBy(optWS ~ "," ~ optWS) ~ "}" ~ SP ~> ParsedAst.Use.UseMany
      }
    }

    def UseOneTag: Rule1[ParsedAst.Use.UseOneTag] = namedRule("SingleTagUse") {
      SP ~ Names.QualifiedType ~ "." ~ Names.Tag ~ SP ~> ParsedAst.Use.UseOneTag
    }

    def UseManyTag: Rule1[ParsedAst.Use.UseManyTag] = {
      def TagAndAlias: Rule1[ParsedAst.Use.NameAndAlias] = namedRule("TagName") {
        SP ~ Names.Tag ~ optional(atomic((WS ~ atomic("=>") ~ WS).named("' => '")) ~ Names.Tag) ~ SP ~> ParsedAst.Use.NameAndAlias
      }

      namedRule("MultipleTagUse") {
        SP ~ Names.QualifiedType ~ atomic(".{") ~ zeroOrMore(TagAndAlias).separatedBy(optWS ~ "," ~ optWS) ~ "}" ~ SP ~> ParsedAst.Use.UseManyTag
      }
    }

    def UseName: Rule1[Name.Ident] = namedRule("Identifier") {
      atomic(Names.LowerCaseName | Names.UpperCaseName | Names.GreekName | Names.MathName | Names.OperatorName).named("Identifier")
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = namedRule("Literal") {
    atomic((Literals.Null | Literals.Bool | Literals.Char | Literals.Str | Literals.Default | Literals.Float | Literals.Int).named("Literal"))
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

    def Char: Rule1[ParsedAst.Literal.Char] = rule {
      SP ~ "'" ~ oneOrMore(!"'" ~ Chars.CharCode) ~ "'" ~ SP ~> ParsedAst.Literal.Char
    }

    // Note that outside of patterns, Strings are parsed as [[Interpolation]]s
    def Str: Rule1[ParsedAst.Literal.Str] = rule {
      SP ~ "\"" ~ zeroOrMore(!"\"" ~ Chars.CharCode) ~ "\"" ~ SP ~> ParsedAst.Literal.Str
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
  // Characters
  /////////////////////////////////////////////////////////////////////////////
  object Chars {

    def Literal: Rule1[ParsedAst.CharCode.Literal] = rule {
      SP ~ !("\\" | EOI) ~ capture(CharPredicate.All) ~ SP ~> ParsedAst.CharCode.Literal
    }

    def Escape: Rule1[ParsedAst.CharCode.Escape] = rule {
      SP ~ "\\" ~ capture(CharPredicate.All) ~ SP ~> ParsedAst.CharCode.Escape
    }

    def CharCode: Rule1[ParsedAst.CharCode] = namedRule("Char") {
      atomic((Escape | Literal).named("Char"))
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  def Expression: Rule1[ParsedAst.Expression] = rule {
    Expressions.Assign
  }

  object Expressions {
    def Stm: Rule1[ParsedAst.Expression] = namedRule("Statement") {
      Expression ~ optional(optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.Stm)
    }

    def Assign: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      PutChannel ~ optional(optWS ~ atomic(":=") ~ optWS ~ PutChannel ~ SP ~> ParsedAst.Expression.Assign)
    }

    def PutChannel: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      LogicalOr ~ zeroOrMore(optWS ~ atomic("<-") ~ optWS ~ LogicalOr ~ SP ~> ParsedAst.Expression.PutChannel)
    }

    def LogicalOr: Rule1[ParsedAst.Expression] = {
      def Or: Rule1[String] = rule {
        WS ~ capture(keyword("or")) ~ WS
      }
      namedRule("Expression") {
        LogicalAnd ~ zeroOrMore(Or ~ LogicalAnd ~ SP ~> ParsedAst.Expression.Binary)
      }
    }

    def LogicalAnd: Rule1[ParsedAst.Expression] = {
      def And: Rule1[String] = rule {
        WS ~ capture(keyword("and")) ~ WS
      }
      namedRule("Expression") {
        BitwiseOr ~ zeroOrMore(And ~ BitwiseOr ~ SP ~> ParsedAst.Expression.Binary)
      }
    }

    def BitwiseOr: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      BitwiseXOr ~ zeroOrMore(optWS ~ capture(atomic("|||")) ~ optWS ~ BitwiseXOr ~ SP ~> ParsedAst.Expression.Binary)
    }

    def BitwiseXOr: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      BitwiseAnd ~ zeroOrMore(optWS ~ capture(atomic("^^^")) ~ optWS ~ BitwiseAnd ~ SP ~> ParsedAst.Expression.Binary)
    }

    def BitwiseAnd: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Equality ~ zeroOrMore(optWS ~ capture(atomic("&&&")) ~ optWS ~ Equality ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Equality: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Relational ~ optional(optWS ~ capture(atomic("==") | atomic("!=") | atomic("<=>")) ~ optWS ~ Relational ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Relational: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Shift ~ optional(WS ~ capture(atomic("<=") | atomic(">=") | "<" | ">") ~ WS ~ Shift ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Shift: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Additive ~ optional(optWS ~ capture(atomic("<<<") | atomic(">>>")) ~ optWS ~ Additive ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Additive: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Multiplicative ~ zeroOrMore(optWS ~ capture("+" | "-") ~ optWS ~ Multiplicative ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Multiplicative: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Compose ~ zeroOrMore(optWS ~ capture(atomic("**") | atomic("*") | atomic("/") | atomic("mod") | atomic("rem")) ~ optWS ~ Compose ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Compose: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Infix ~ zeroOrMore(optWS ~ atomic("<+>") ~ optWS ~ Infix ~ SP ~> ParsedAst.Expression.FixpointCompose)
    }

    def Infix: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Special ~ zeroOrMore(optWS ~ "`" ~ FName ~ "`" ~ optWS ~ Special ~ SP ~> ParsedAst.Expression.Infix)
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

      namedRule("Expression") {
        // NB: UserOpN must occur before UserOp2.
        Unary ~ zeroOrMore(optWS ~ atomic((UserOpN | UserOp3 | UserOp2 | MathOp).named("UserOperator")) ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Binary)
      }
    }

    def Unary: Rule1[ParsedAst.Expression] = {
      def UnaryOp1: Rule1[String] = rule {
        capture("+" | "-" | atomic("~~~"))
      }
      def UnaryOp2: Rule1[String] = rule {
        capture(keyword("not")) ~ WS
      }
      namedRule("Expression") {
        !Literal ~ (SP ~ atomic((UnaryOp1 | UnaryOp2).named("UnaryOperator")) ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Unary) | Ref
      }
    }

    // TODO: Why are these not primary?
    def Ref: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      (SP ~ keyword("ref") ~ WS ~ Ref ~ optional(WS ~ keyword("@") ~ WS ~ Expression) ~ SP ~> ParsedAst.Expression.Ref) | Deref
    }

    def Deref: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      (SP ~ keyword("deref") ~ WS ~ Deref ~ SP ~> ParsedAst.Expression.Deref) | Cast
    }

    def Cast: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Ascribe ~ optional(WS ~ keyword("as") ~ WS ~ TypAndEffFragment ~ SP ~> ParsedAst.Expression.Cast)
    }

    def Ascribe: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      FAppend ~ optional(optWS ~ ":" ~ optWS ~ TypAndEffFragment ~ SP ~> ParsedAst.Expression.Ascribe)
    }

    def TypAndEffFragment: Rule2[Option[ParsedAst.Type], Option[ParsedAst.Type]] = {
      def SomeTyp: Rule1[Option[ParsedAst.Type]] = namedRule("Type") {
        Type ~> ((tpe: ParsedAst.Type) => Some(tpe))
      }

      def SomeEff: Rule1[Option[ParsedAst.Type]] = namedRule("Effect") {
        Type ~> ((tpe: ParsedAst.Type) => Some(tpe))
      }

      def TypOnly: Rule2[Option[ParsedAst.Type], Option[ParsedAst.Type]] = namedRule("Type") {
        SomeTyp ~ push(None)
      }

      def EffOnly: Rule2[Option[ParsedAst.Type], Option[ParsedAst.Type]] = namedRule("Type") {
        push(None) ~ "&" ~ WS ~ SomeEff
      }

      def TypAndEff: Rule2[Option[ParsedAst.Type], Option[ParsedAst.Type]] = namedRule("Type") {
        SomeTyp ~ WS ~ "&" ~ WS ~ SomeEff
      }

      namedRule("Type") {
        TypAndEff | TypOnly | EffOnly
      }
    }

    def Primary: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      LetRegion | LetMatch | LetMatchStar | LetUse | LetImport | IfThenElse | Reify | ReifyBool | ReifyType | Choose | Match | LambdaMatch | TryCatch | Lambda | Tuple |
        RecordOperation | RecordLiteral | Block | RecordSelectLambda | NewChannel |
        GetChannel | SelectChannel | Spawn | Lazy | Force | Intrinsic | ArrayLit | ArrayNew |
        FNil | FSet | FMap | ConstraintSet | FixpointProject | FixpointSolveWithProject | FixpointQueryWithSelect |
        ConstraintSingleton | Interpolation | Literal | Existential | Universal |
        UnaryLambda | FName | Tag | Hole
    }

    def Literal: Rule1[ParsedAst.Expression.Lit] = namedRule("Expression") {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Expression.Lit
    }

    def Interpolation: Rule1[ParsedAst.Expression.Interpolation] = {
      def ExpPart: Rule1[ParsedAst.InterpolationPart] = namedRule("Char") {
        SP ~ atomic("${") ~ optWS ~ optional(Expression) ~ optWS ~ "}" ~ SP ~> ParsedAst.InterpolationPart.ExpPart
      }

      def StrPart: Rule1[ParsedAst.InterpolationPart] = namedRule("Char") {
        SP ~ oneOrMore(!("\"" | atomic("${")) ~ Chars.CharCode) ~ SP ~> ParsedAst.InterpolationPart.StrPart
      }

      def InterpolationPart: Rule1[ParsedAst.InterpolationPart] = namedRule("Char") {
        ExpPart | StrPart
      }

      namedRule("Expression") {
        SP ~ "\"" ~ zeroOrMore(InterpolationPart) ~ "\"" ~ SP ~> ParsedAst.Expression.Interpolation
      }
    }

    def IfThenElse: Rule1[ParsedAst.Expression.IfThenElse] = namedRule("Expression") {
      SP ~ keyword("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ WS ~ keyword("else") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
    }

    def Reify: Rule1[ParsedAst.Expression.Reify] = namedRule("Expression") {
      SP ~ keyword("reify") ~ WS ~ Type ~ SP ~> ParsedAst.Expression.Reify
    }

    def ReifyBool: Rule1[ParsedAst.Expression.ReifyBool] = namedRule("Expression") {
      SP ~ keyword("reifyBool") ~ WS ~ Type ~ SP ~> ParsedAst.Expression.ReifyBool
    }

    def ReifyType: Rule1[ParsedAst.Expression.ReifyType] = namedRule("Expression") {
      SP ~ keyword("reifyType") ~ WS ~ Type ~ SP ~> ParsedAst.Expression.ReifyType
    }

    def LetMatch: Rule1[ParsedAst.Expression.LetMatch] = namedRule("Expression") {
      SP ~ keyword("let") ~ WS ~ Modifiers ~ Pattern ~ optWS ~ optional(":" ~ optWS ~ Type ~ optWS) ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetMatch
    }

    def LetMatchStar: Rule1[ParsedAst.Expression.LetMatchStar] = namedRule("Expression") {
      SP ~ keyword("let*") ~ WS ~ Pattern ~ optWS ~ optional(":" ~ optWS ~ Type ~ optWS) ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetMatchStar
    }

    def LetUse: Rule1[ParsedAst.Expression.Use] = namedRule("Expression") {
      SP ~ Use ~ optWS ~ ";" ~ optWS ~ Expressions.Stm ~ SP ~> ParsedAst.Expression.Use
    }

    def LetRegion: Rule1[ParsedAst.Expression.LetRegion] = namedRule("Expression") {
      SP ~ keyword("let region") ~ WS ~ Names.Variable ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetRegion
    }

    def LetImport: Rule1[ParsedAst.Expression] = {

      def JvmStaticName: Rule1[Seq[String]] = rule {
        Names.JavaName ~ ":" ~ Names.JavaIdentifier ~> ((xs: Seq[String], x: String) => xs :+ x)
      }

      def Constructor: Rule1[ParsedAst.JvmOp] = rule {
        keyword("new") ~ WS ~ Names.JavaName ~ optWS ~ Signature ~ WS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.Constructor
      }

      def Method: Rule1[ParsedAst.JvmOp] = rule {
        Names.JavaName ~ optWS ~ Signature ~ optional(WS ~ keyword("as") ~ WS ~ Names.Variable) ~> ParsedAst.JvmOp.Method
      }

      def StaticMethod: Rule1[ParsedAst.JvmOp] = rule {
        JvmStaticName ~ optWS ~ Signature ~ optional(WS ~ keyword("as") ~ WS ~ Names.Variable) ~> ParsedAst.JvmOp.StaticMethod
      }

      def GetField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("get") ~ WS ~ Names.JavaName ~ WS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.GetField
      }

      def PutField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("set") ~ WS ~ Names.JavaName ~ WS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.PutField
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

      namedRule("Expression") {
        SP ~ Import ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetImport
      }
    }

    def Match: Rule1[ParsedAst.Expression.Match] = {
      def Rule: Rule1[ParsedAst.MatchRule] = rule {
        keyword("case") ~ WS ~ Pattern ~ optWS ~ optional(keyword("if") ~ WS ~ Expression ~ optWS) ~ atomic("=>") ~ optWS ~ Stm ~> ParsedAst.MatchRule
      }

      namedRule("Expression") {
        SP ~ keyword("match") ~ WS ~ Expression ~ optWS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(CaseSeparator) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Match
      }
    }

    def Choose: Rule1[ParsedAst.Expression.Choose] = {
      def MatchOne: Rule1[Seq[ParsedAst.Expression]] = namedRule("Expression") {
        Expression ~> ((e: ParsedAst.Expression) => Seq(e))
      }

      def MatchMany: Rule1[Seq[ParsedAst.Expression]] = namedRule("'('") {
        "(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
      }

      def ChoicePattern: Rule1[ParsedAst.ChoicePattern] = rule {
        (SP ~ "_" ~ SP ~> ParsedAst.ChoicePattern.Wild) |
          (SP ~ keyword("Absent") ~ SP ~> ParsedAst.ChoicePattern.Absent) |
          (SP ~ keyword("Present") ~ optWS ~ "(" ~ Names.Variable ~ ")" ~ SP ~> ParsedAst.ChoicePattern.Present)
      }

      def CaseOne: Rule1[ParsedAst.ChoiceRule] = namedRule("ChooseCase") {
        SP ~ keyword("case") ~ WS ~ ChoicePattern ~ WS ~ atomic("=>") ~ WS ~ Expression ~ SP ~>
          ((sp1: SourcePosition, x: ParsedAst.ChoicePattern, e: ParsedAst.Expression, sp2: SourcePosition) => ParsedAst.ChoiceRule(sp1, Seq(x), e, sp2))
      }

      def CaseMany: Rule1[ParsedAst.ChoiceRule] = namedRule("ChooseCase") {
        SP ~ keyword("case") ~ WS ~ "(" ~ optWS ~ oneOrMore(ChoicePattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ WS ~ atomic("=>") ~ WS ~ Expression ~ SP ~> ParsedAst.ChoiceRule
      }

      def ChooseKind: Rule1[Boolean] = rule {
        (keyword("choose*") ~ push(true)) | (keyword("choose") ~ push(false))
      }

      namedRule("Expression") {
        SP ~ ChooseKind ~ WS ~ (MatchMany | MatchOne) ~ optWS ~ "{" ~ optWS ~ oneOrMore(CaseMany | CaseOne).separatedBy(WS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Choose
      }
    }

    def TryCatch: Rule1[ParsedAst.Expression] = {
      def CatchRule: Rule1[ParsedAst.CatchRule] = namedRule("CatchCase") {
        keyword("case") ~ WS ~ Names.Variable ~ optWS ~ ":" ~ optWS ~ atomic("##") ~ Names.JavaName ~ WS ~ atomic("=>") ~ optWS ~ Expression ~> ParsedAst.CatchRule
      }

      def CatchBody: Rule1[Seq[ParsedAst.CatchRule]] = namedRule("'{'") {
        "{" ~ optWS ~ oneOrMore(CatchRule).separatedBy(CaseSeparator) ~ optWS ~ "}"
      }

      namedRule("Expression") {
        SP ~ keyword("try") ~ WS ~ Expression ~ optWS ~ keyword("catch") ~ optWS ~ CatchBody ~ SP ~> ParsedAst.Expression.TryCatch
      }
    }

    def RecordSelect: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Postfix ~ zeroOrMore(optWS ~ "." ~ Names.Field ~ SP ~> ParsedAst.Expression.RecordSelect)
    }

    //TODO SJ: order this with primaries
    def NewChannel: Rule1[ParsedAst.Expression.NewChannel] = namedRule("Expression") {
      SP ~ keyword("chan") ~ WS ~ Type ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.NewChannel
    }

    def GetChannel: Rule1[ParsedAst.Expression.GetChannel] = namedRule("Expression") {
      SP ~ atomic("<-") ~ WS ~ RecordSelect ~ SP ~> ParsedAst.Expression.GetChannel
    }

    def SelectChannel: Rule1[ParsedAst.Expression.SelectChannel] = {
      def SelectChannelRule: Rule1[ParsedAst.SelectChannelRule] = namedRule("SelectCase") {
        keyword("case") ~ WS ~ Names.Variable ~ optWS ~ atomic("<-") ~ optWS ~ Expression ~ optWS ~ atomic("=>") ~ optWS ~ Stm ~> ParsedAst.SelectChannelRule
      }

      def SelectChannelDefault: Rule1[ParsedAst.Expression] = namedRule("SelectCase") {
        keyword("case") ~ WS ~ "_" ~ optWS ~ atomic("=>") ~ optWS ~ Stm
      }

      namedRule("Expression") {
        SP ~ keyword("select") ~ WS ~ "{" ~ optWS ~ oneOrMore(SelectChannelRule).separatedBy(CaseSeparator) ~ optWS ~ optional(SelectChannelDefault) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.SelectChannel
      }
    }

    def Spawn: Rule1[ParsedAst.Expression.Spawn] = namedRule("Expression") {
      SP ~ keyword("spawn") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Spawn
    }

    def Lazy: Rule1[ParsedAst.Expression.Lazy] = namedRule("Expression") {
      SP ~ keyword("lazy") ~ WS ~ RecordSelect ~ SP ~> ParsedAst.Expression.Lazy
    }

    def Force: Rule1[ParsedAst.Expression.Force] = namedRule("Expression") {
      SP ~ keyword("force") ~ WS ~ RecordSelect ~ SP ~> ParsedAst.Expression.Force
    }

    def Intrinsic: Rule1[ParsedAst.Expression.Intrinsic] = namedRule("Expression") {
      SP ~ "$"~ Names.Intrinsic ~ "$" ~ ArgumentList ~ SP ~> ParsedAst.Expression.Intrinsic
    }

    def Postfix: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      ArraySlice ~ zeroOrMore(optWS ~ "." ~ Names.Definition ~ ArgumentList ~ SP ~> ParsedAst.Expression.Postfix)
    }

    def ArraySlice: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      ArrayLoad ~ optional(optWS ~ "[" ~ optWS ~ optional(Expression) ~ optWS ~ atomic("..") ~ optWS ~ optional(Expression) ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArraySlice)
    }

    def ArrayLoad: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      ArrayStore ~ zeroOrMore(optWS ~ "[" ~ optWS ~ Expression ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArrayLoad)
    }

    def ArrayStore: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Apply ~ optional(oneOrMore(optWS ~ "[" ~ optWS ~ Expression ~ optWS ~ "]") ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.ArrayStore)
    }

    def Apply: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      Primary ~ zeroOrMore(ArgumentList ~ SP ~> ParsedAst.Expression.Apply)
    }

    def Tag: Rule1[ParsedAst.Expression.Tag] = namedRule("Expression") {
      SP ~ Names.QualifiedTag ~ optional(optWS ~ Tuple.named("Tuple")) ~ SP ~> ParsedAst.Expression.Tag
    }

    def Tuple: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      SP ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Tuple
    }

    def Block: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      "{" ~ optWS ~ Stm ~ optWS ~ "}"
    }

    def RecordLiteral: Rule1[ParsedAst.Expression] = {
      def FieldLit: Rule1[ParsedAst.RecordField] = namedRule("FieldLiteral") {
        SP ~ Names.Field ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.RecordField
      }

      namedRule("Expression") {
        SP ~ "{" ~ optWS ~ zeroOrMore(FieldLit).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.RecordLit
      }
    }

    def RecordSelectLambda: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      SP ~ "." ~ Names.Field ~ SP ~> ParsedAst.Expression.RecordSelectLambda
    }

    def RecordOperation: Rule1[ParsedAst.Expression] = {
      def RecordOp: Rule1[ParsedAst.RecordOp] = namedRule("RecordOperator") {
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

      namedRule("Expression") {
        SP ~ "{" ~ optWS ~ oneOrMore(RecordOp).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "|" ~ optWS ~ Expression ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.RecordOperation
      }
    }

    def ArrayLit: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      SP ~ "[" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArrayLit
    }

    def ArrayNew: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      SP ~ "[" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Expression ~ optWS ~ "]" ~ SP ~> ParsedAst.Expression.ArrayNew
    }

    def FNil: Rule1[ParsedAst.Expression.FNil] = namedRule("Expression") {
      SP ~ keyword("Nil") ~ SP ~> ParsedAst.Expression.FNil
    }

    def FAppend: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      FList ~ optional(optWS ~ SP ~ atomic(":::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FAppend)
    }

    def FList: Rule1[ParsedAst.Expression] = namedRule("Expression") {
      RecordSelect ~ optional(optWS ~ SP ~ atomic("::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FCons)
    }

    def FSet: Rule1[ParsedAst.Expression.FSet] = namedRule("Expression") {
      SP ~ "Set#{" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.FSet
    }

    def FMap: Rule1[ParsedAst.Expression.FMap] = {
      def KeyValue: Rule1[(ParsedAst.Expression, ParsedAst.Expression)] = rule {
        Expression ~ optWS ~ atomic("->") ~ optWS ~ Expression ~> ((e1: ParsedAst.Expression, e2: ParsedAst.Expression) => (e1, e2))
      }

      namedRule("Expression") {
        SP ~ "Map#{" ~ optWS ~ zeroOrMore(KeyValue).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.FMap
      }
    }

    def FName: Rule1[ParsedAst.Expression] = namedRule("FunctionName") {
      SName | QName
    }

    def SName: Rule1[ParsedAst.Expression.SName] = namedRule("FunctionName") {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Expression.SName
    }

    def QName: Rule1[ParsedAst.Expression.QName] = namedRule("FunctionName") {
      SP ~ Names.QualifiedDefinition ~ SP ~> ParsedAst.Expression.QName
    }

    def Hole: Rule1[ParsedAst.Expression.Hole] = {
      def AnonymousHole: Rule1[ParsedAst.Expression.Hole] = rule {
        SP ~ atomic("???") ~ SP ~> ((sp1: SourcePosition, sp2: SourcePosition) => ParsedAst.Expression.Hole(sp1, None, sp2))
      }

      def NamedHole: Rule1[ParsedAst.Expression.Hole] = rule {
        SP ~ "?" ~ Names.Hole ~ SP ~> ((sp1: SourcePosition, name: Name.Ident, sp2: SourcePosition) => ParsedAst.Expression.Hole(sp1, Some(name), sp2))
      }

      rule {
        AnonymousHole | NamedHole
      }
    }

    def UnaryLambda: Rule1[ParsedAst.Expression.Lambda] = namedRule("Expression") {
      SP ~ FormalParam ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ((sp1: SourcePosition, param: ParsedAst.FormalParam, body: ParsedAst.Expression, sp2: SourcePosition) =>
        ParsedAst.Expression.Lambda(sp1, Seq(param), body, sp2))
    }

    def Lambda: Rule1[ParsedAst.Expression.Lambda] = namedRule("Expression") {
      SP ~ FormalParamList ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Lambda
    }

    def LambdaMatch: Rule1[ParsedAst.Expression.LambdaMatch] = namedRule("Expression") {
      SP ~ keyword("match") ~ optWS ~ Pattern ~ optWS ~ atomic("->") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.LambdaMatch
    }

    def ConstraintSingleton: Rule1[ParsedAst.Expression] = namedRule("ConstraintSet") {
      atomic("#") ~ optWS ~ SP ~ Constraint ~ SP ~> ParsedAst.Expression.FixpointConstraint
    }

    def ConstraintSet: Rule1[ParsedAst.Expression] = namedRule("ConstraintSet") {
      SP ~ atomic("#{") ~ optWS ~ zeroOrMore(Constraint) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.FixpointConstraintSet
    }

    def FixpointProject: Rule1[ParsedAst.Expression] = {
      def ExpressionPart: Rule1[Seq[ParsedAst.Expression]] = namedRule("Expression") {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      def ProjectPart: Rule1[Seq[Name.Ident]] = namedRule("PredicateName") {
        oneOrMore(Names.Predicate).separatedBy(optWS ~ "," ~ optWS)
      }

      namedRule("Projection") {
        SP ~ keyword("project") ~ WS ~ ExpressionPart ~ WS ~ keyword("into") ~ WS ~ ProjectPart ~ SP ~> ParsedAst.Expression.FixpointProjectInto
      }
    }

    def FixpointSolveWithProject: Rule1[ParsedAst.Expression] = {
      def ExpressionPart: Rule1[Seq[ParsedAst.Expression]] = namedRule("Expression") {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      def ProjectPart: Rule1[Option[Seq[Name.Ident]]] = namedRule("' project'") {
        optional(WS ~ keyword("project") ~ WS ~ oneOrMore(Names.Predicate).separatedBy(optWS ~ "," ~ optWS))
      }

      namedRule("Solve") {
        SP ~ keyword("solve") ~ WS ~ ExpressionPart ~ ProjectPart ~ SP ~> ParsedAst.Expression.FixpointSolveWithProject
      }
    }

    def FixpointQueryWithSelect: Rule1[ParsedAst.Expression] = {
      def ExpressionPart: Rule1[Seq[ParsedAst.Expression]] = namedRule("Expression") {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      def SelectPart: Rule1[ParsedAst.SelectFragment] = {
        def SelectOne: Rule1[ParsedAst.SelectFragment] = namedRule("Expression") {
          Expression ~> ((e: ParsedAst.Expression) => ParsedAst.SelectFragment(Seq(e), None))
        }

        def SelectMany: Rule1[ParsedAst.SelectFragment] = {
          def TermList: Rule1[Seq[ParsedAst.Expression]] = namedRule("Expression") {
            zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
          }

          def LatTerm: Rule1[Option[ParsedAst.Expression]] = namedRule("LatticeTerm") {
            optional(optWS ~ ";" ~ optWS ~ Expression)
          }

          namedRule("SelectTuple") {
            "(" ~ optWS ~ TermList ~ LatTerm ~ optWS ~ ")" ~> ParsedAst.SelectFragment
          }
        }

        namedRule("' select'") {
          WS ~ keyword("select") ~ WS ~ (SelectMany | SelectOne)
        }
      }

      def FromPart: Rule1[Seq[ParsedAst.Predicate.Body.Atom]] = namedRule("' from'") {
        WS ~ keyword("from") ~ WS ~ oneOrMore(Predicates.Body.Positive | Predicates.Body.Negative).separatedBy(optWS ~ "," ~ optWS)
      }

      def WherePart: Rule1[Option[ParsedAst.Expression]] = namedRule("' where'") {
        optional(WS ~ keyword("where") ~ WS ~ Expression)
      }

      namedRule("Query") {
        SP ~ keyword("query") ~ WS ~ ExpressionPart ~ SelectPart ~ FromPart ~ WherePart ~ SP ~> ParsedAst.Expression.FixpointQueryWithSelect
      }
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

    def Simple: Rule1[ParsedAst.Pattern] = namedRule("Pattern") {
      FNil | Tag | Lit | Tuple | Array | ArrayTailSpread | ArrayHeadSpread | Var
    }

    def Var: Rule1[ParsedAst.Pattern.Var] = namedRule("Pattern") {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Pattern.Var
    }

    def Lit: Rule1[ParsedAst.Pattern.Lit] = namedRule("Pattern") {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Pattern.Lit
    }

    def Tag: Rule1[ParsedAst.Pattern.Tag] = namedRule("Pattern") {
      SP ~ Names.QualifiedTag ~ optional(optWS ~ Tuple) ~ SP ~> ParsedAst.Pattern.Tag
    }

    def Tuple: Rule1[ParsedAst.Pattern.Tuple] = namedRule("Pattern") {
      SP ~ "(" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Pattern.Tuple
    }

    def Array: Rule1[ParsedAst.Pattern.Array] = namedRule("Pattern") {
      SP ~ "[" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.Pattern.Array
    }

    def ArrayTailSpread: Rule1[ParsedAst.Pattern.ArrayTailSpread] = namedRule("Pattern") {
      SP ~ "[" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "," ~ optWS ~ ".." ~ Names.Variable ~ optWS ~ "]" ~ SP ~> ParsedAst.Pattern.ArrayTailSpread
    }

    def ArrayHeadSpread: Rule1[ParsedAst.Pattern.ArrayHeadSpread] = namedRule("Pattern") {
      SP ~ "[" ~ optWS ~ Names.Variable ~ ".." ~ optWS ~ "," ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ SP ~> ParsedAst.Pattern.ArrayHeadSpread
    }

    def FNil: Rule1[ParsedAst.Pattern.FNil] = namedRule("Pattern") {
      SP ~ keyword("Nil") ~ SP ~> ParsedAst.Pattern.FNil
    }

    def FList: Rule1[ParsedAst.Pattern] = namedRule("Pattern") {
      Simple ~ optional(optWS ~ SP ~ atomic("::") ~ SP ~ optWS ~ Pattern ~> ParsedAst.Pattern.FCons)
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Constraints                                                             //
  /////////////////////////////////////////////////////////////////////////////
  def Constraint: Rule1[ParsedAst.Constraint] = {
    def Head: Rule1[ParsedAst.Predicate.Head] = rule {
      HeadPredicate
    }

    def Body: Rule1[Seq[ParsedAst.Predicate.Body]] = rule {
      optional(optWS ~ ":-" ~ optWS ~ oneOrMore(BodyPredicate).separatedBy(optWS ~ "," ~ optWS)) ~> ((o: Option[Seq[ParsedAst.Predicate.Body]]) => o.getOrElse(Seq.empty))
    }

    rule {
      optWS ~ SP ~ Head ~ Body ~ optWS ~ "." ~ SP ~> ParsedAst.Constraint
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Predicates                                                              //
  /////////////////////////////////////////////////////////////////////////////
  def HeadPredicate: Rule1[ParsedAst.Predicate.Head] = rule {
    Predicates.Head.Atom
  }

  def BodyPredicate: Rule1[ParsedAst.Predicate.Body] = rule {
    Predicates.Body.Positive | Predicates.Body.Negative | Predicates.Body.Guard | Predicates.Body.Filter
  }

  object Predicates {

    object Head {

      def Atom: Rule1[ParsedAst.Predicate.Head.Atom] = namedRule("Atom") {
        SP ~ Names.Predicate ~ optWS ~ Predicates.TermList ~ SP ~> ParsedAst.Predicate.Head.Atom
      }

    }

    object Body {

      def Positive: Rule1[ParsedAst.Predicate.Body.Atom] = namedRule("Atom") {
        SP ~ push(Polarity.Positive) ~ Names.Predicate ~ optWS ~ Predicates.PatternList ~ SP ~> ParsedAst.Predicate.Body.Atom
      }

      def Negative: Rule1[ParsedAst.Predicate.Body.Atom] = {
        def Not: Rule0 = rule {
          (keyword("not") ~ WS)
        }

        namedRule("Atom") {
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

    def UnaryArrow: Rule1[ParsedAst.Type] = namedRule("Type") {
      Or ~ optional(
        (optWS ~ atomic("~>") ~ optWS ~ Type ~ SP ~> ParsedAst.Type.UnaryImpureArrow) |
          (optWS ~ atomic("->") ~ optWS ~ Type ~ optional(WS ~ "&" ~ WS ~ Type) ~ SP ~> ParsedAst.Type.UnaryPolymorphicArrow)
      )
    }

    def Or: Rule1[ParsedAst.Type] = namedRule("Type") {
      And ~ zeroOrMore(WS ~ keyword("or") ~ WS ~ Type ~ SP ~> ParsedAst.Type.Or)
    }

    def And: Rule1[ParsedAst.Type] = namedRule("Type") {
      Ascribe ~ zeroOrMore(WS ~ keyword("and") ~ WS ~ Type ~ SP ~> ParsedAst.Type.And)
    }

    def Ascribe: Rule1[ParsedAst.Type] = namedRule("Type") {
      Apply ~ optional(WS ~ keyword(":") ~ WS ~ Kind ~ SP ~> ParsedAst.Type.Ascribe)
    }

    def Apply: Rule1[ParsedAst.Type] = namedRule("Type") {
      Primary ~ zeroOrMore(TypeArguments ~ SP ~> ParsedAst.Type.Apply)
    }

    def Primary: Rule1[ParsedAst.Type] = namedRule("Type") {
      Arrow | Tuple | Record | RecordRow | Schema | SchemaRow | Native | True | False | Pure | Impure | Not | Var | Ambiguous
    }

    def Arrow: Rule1[ParsedAst.Type] = {
      def TypeList: Rule1[Seq[ParsedAst.Type]] = rule {
        "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
      }

      namedRule("Type") {
        SP ~ TypeList ~ optWS ~ (
          (atomic("~>") ~ optWS ~ Type ~ SP ~> ParsedAst.Type.ImpureArrow) |
            (atomic("->") ~ optWS ~ Type ~ optional(WS ~ "&" ~ WS ~ Type) ~ SP ~> ParsedAst.Type.PolymorphicArrow)
          )
      }
    }

    def Tuple: Rule1[ParsedAst.Type] = {
      def Singleton: Rule1[ParsedAst.Type] = namedRule("Type") {
        "(" ~ optWS ~ Type ~ optWS ~ ")"
      }

      def Tuple: Rule1[ParsedAst.Type] = namedRule("Type") {
        SP ~ "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Type.Tuple
      }

      namedRule("Type") {
        Singleton | Tuple
      }
    }

    def Record: Rule1[ParsedAst.Type] = namedRule("Type") {
      SP ~ "{" ~ optWS ~ zeroOrMore(RecordFieldType).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ "}" ~ SP ~> ParsedAst.Type.Record
    }

    def RecordRow: Rule1[ParsedAst.Type] = namedRule("Type") {
      SP ~ "(" ~ optWS ~ zeroOrMore(RecordFieldType).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ ")" ~ SP ~> ParsedAst.Type.RecordRow
    }

    private def RecordFieldType: Rule1[ParsedAst.RecordFieldType] = rule {
      SP ~ Names.Field ~ optWS ~ "::" ~ optWS ~ Type ~ SP ~> ParsedAst.RecordFieldType
    }

    def Schema: Rule1[ParsedAst.Type] = rule {
      SP ~ atomic("#{") ~ optWS ~ zeroOrMore(RelPredicateWithTypes | LatPredicateWithTypes | PredicateWithAlias).separatedBy(optWS ~ "," ~ optWS) ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ "}" ~ SP ~> ParsedAst.Type.Schema
    }

    def SchemaRow: Rule1[ParsedAst.Type] = rule {
      SP ~ atomic("#(") ~ optWS ~ zeroOrMore(RelPredicateWithTypes | LatPredicateWithTypes | PredicateWithAlias).separatedBy(optWS ~ "," ~ optWS) ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ ")" ~ SP ~> ParsedAst.Type.SchemaRow
    }

    private def PredicateWithAlias: Rule1[ParsedAst.PredicateType.PredicateWithAlias] = rule {
      SP ~ Names.QualifiedPredicate ~ optional(TypeArguments) ~ SP ~> ParsedAst.PredicateType.PredicateWithAlias
    }

    private def RelPredicateWithTypes: Rule1[ParsedAst.PredicateType.RelPredicateWithTypes] = rule {
      SP ~ Names.Predicate ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.PredicateType.RelPredicateWithTypes
    }

    private def LatPredicateWithTypes: Rule1[ParsedAst.PredicateType.LatPredicateWithTypes] = rule {
      SP ~ Names.Predicate ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ";" ~ optWS ~ Type ~ optWS ~ ")" ~ SP ~> ParsedAst.PredicateType.LatPredicateWithTypes
    }

    def Native: Rule1[ParsedAst.Type] = rule {
      SP ~ atomic("##") ~ Names.JavaName ~ SP ~> ParsedAst.Type.Native
    }

    def True: Rule1[ParsedAst.Type] = namedRule("Type") {
      SP ~ keyword("true") ~ SP ~> ParsedAst.Type.True
    }

    def False: Rule1[ParsedAst.Type] = namedRule("Type") {
      SP ~ keyword("false") ~ SP ~> ParsedAst.Type.False
    }

    def Pure: Rule1[ParsedAst.Type] = namedRule("Type") {
      SP ~ keyword("Pure") ~ SP ~> ParsedAst.Type.True
    }

    def Impure: Rule1[ParsedAst.Type] = namedRule("Type") {
      SP ~ keyword("Impure") ~ SP ~> ParsedAst.Type.False
    }

    def Not: Rule1[ParsedAst.Type] = namedRule("Type") {
      // NB: We must not use Type here because it gives the wrong precedence.
      SP ~ keyword("not") ~ WS ~ Apply ~ SP ~> ParsedAst.Type.Not
    }

    def Var: Rule1[ParsedAst.Type] = namedRule("Type") {
      SP ~ atomic(Names.Variable.named("TypeVariable")) ~ SP ~> ParsedAst.Type.Var
    }

    def Ambiguous: Rule1[ParsedAst.Type] = namedRule("Type") {
      SP ~ Names.QualifiedType ~ SP ~> ParsedAst.Type.Ambiguous
    }

    private def TypeArguments: Rule1[Seq[ParsedAst.Type]] = namedRule("TypeArgumentList") {
      "[" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]"
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Kinds                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def Kind: Rule1[ParsedAst.Kind] = rule {
    Kinds.Arrow | Kinds.SimpleKind
  }

  object Kinds {

    def SimpleKind: Rule1[ParsedAst.Kind] = namedRule("Kind") {
      Kinds.Star | Kinds.Bool | Kinds.Record | Kinds.Schema | Kinds.Parens
    }

    def Arrow: Rule1[ParsedAst.Kind] = rule {
      SimpleKind ~ optional(optWS ~ atomic("->") ~ optWS ~ Kind ~ SP ~> ParsedAst.Kind.Arrow)
    }

    def Star: Rule1[ParsedAst.Kind.Star] = rule {
      SP ~ keyword("Type") ~ SP ~> ParsedAst.Kind.Star
    }

    def Bool: Rule1[ParsedAst.Kind.Bool] = rule {
      SP ~ keyword("Bool") ~ SP ~> ParsedAst.Kind.Bool
    }

    def Record: Rule1[ParsedAst.Kind.RecordRow] = rule {
      SP ~ keyword("RecordRow") ~ SP ~> ParsedAst.Kind.RecordRow
    }

    def Schema: Rule1[ParsedAst.Kind.SchemaRow] = rule {
      SP ~ keyword("SchemaRow") ~ SP ~> ParsedAst.Kind.SchemaRow
    }

    def Predicate: Rule1[ParsedAst.Kind.Predicate] = rule {
      SP ~ keyword("Predicate") ~ SP ~> ParsedAst.Kind.Predicate
    }

    def Parens: Rule1[ParsedAst.Kind] = namedRule("Parenthesis") {
      "(" ~ Kind ~ ")"
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Helpers                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  def FormalParam: Rule1[ParsedAst.FormalParam] = namedRule("FormalParameter") {
    // TODO: A quick hack to allow mut annotations.
    SP ~ Modifiers ~ Names.Variable ~ optional(optWS ~ ":" ~ optWS ~ optional(keyword("mut") ~ WS) ~ Type) ~ SP ~> ParsedAst.FormalParam
  }

  def FormalParamList: Rule1[Seq[ParsedAst.FormalParam]] = namedRule("FormalParameterList") {
    "(" ~ optWS ~ zeroOrMore(FormalParam).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def Argument: Rule1[ParsedAst.Argument] = {
    def NamedArgument: Rule1[ParsedAst.Argument] = rule {
      Names.Field ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Argument.Named
    }

    def UnnamedArgument: Rule1[ParsedAst.Argument] = rule {
      Expression ~> ParsedAst.Argument.Unnamed
    }

    rule {
      NamedArgument | UnnamedArgument
    }
  }

  def ArgumentList: Rule1[Seq[ParsedAst.Argument]] = rule {
    "(" ~ optWS ~ zeroOrMore(Argument).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def OptArgumentList: Rule1[Option[Seq[ParsedAst.Argument]]] = namedRule("OptionalArgumentList") {
    optional(ArgumentList)
  }

  def AttributeList: Rule1[Seq[ParsedAst.Attribute]] = rule {
    "(" ~ optWS ~ zeroOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def Annotations: Rule1[Seq[ParsedAst.Annotation]] = namedRule("Annotation") {
    zeroOrMore(Annotation).separatedBy(WS) ~ optWS
  }

  def Modifiers: Rule1[Seq[ParsedAst.Modifier]] = {
    def Inline: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("inline")) ~ SP ~> ParsedAst.Modifier
    }

    def Lawless: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("lawless")) ~ SP ~> ParsedAst.Modifier
    }

    def Override: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("override")) ~ SP ~> ParsedAst.Modifier
    }

    def Public: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("pub")) ~ SP ~> ParsedAst.Modifier
    }

    def Sealed: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("sealed")) ~ SP ~> ParsedAst.Modifier
    }

    def Scoped: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("scoped")) ~ SP ~> ParsedAst.Modifier
    }

    def Unlawful: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("unlawful")) ~ SP ~> ParsedAst.Modifier
    }

    def Modifier: Rule1[ParsedAst.Modifier] = rule {
      Inline | Lawless | Override | Public | Sealed | Scoped | Unlawful
    }

    namedRule("Modifier") {
      zeroOrMore(Modifier).separatedBy(WS) ~ optWS
    }
  }

  def Annotation: Rule1[ParsedAst.Annotation] = rule {
    SP ~ "@" ~ Names.Annotation ~ OptArgumentList ~ SP ~> ParsedAst.Annotation
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
    val LegalLetter: CharPredicate = CharPredicate.AlphaNum ++ "_" ++ "!"

    /**
      * A lowercase identifier is a lowercase letter optionally followed by any letter, underscore, or prime.
      */
    def LowerCaseName: Rule1[Name.Ident] = rule {
      atomic((SP ~ capture(optional("_") ~ LowerLetter ~ zeroOrMore(LegalLetter)) ~ SP).named("LowerCaseName")) ~> Name.Ident
    }

    /**
      * An uppercase identifier is an uppercase letter optionally followed by any letter, underscore, or prime.
      */
    def UpperCaseName: Rule1[Name.Ident] = rule {
      atomic((SP ~ capture(optional("_") ~ UpperLetter ~ zeroOrMore(LegalLetter)) ~ SP).named("UpperCaseName")) ~> Name.Ident
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
      * A wildcard identifier.
      */
    def Wildcard: Rule1[Name.Ident] = rule {
      SP ~ capture("_") ~ SP ~> Name.Ident
    }

    /**
      * A greek identifier.
      */
    def GreekName: Rule1[Name.Ident] = rule {
      atomic((SP ~ capture(oneOrMore(GreekLetter)) ~ SP).named("GreekName")) ~> Name.Ident
    }

    /**
      * A math identifier.
      */
    def MathName: Rule1[Name.Ident] = namedRule("MathSymbol") {
      atomic((SP ~ capture(oneOrMore(MathLetter)) ~ SP).named("MathSymbol")) ~> Name.Ident
    }

    /**
      * An operator identifier.
      */
    def OperatorName: Rule1[Name.Ident] = rule {
      atomic((SP ~ capture(oneOrMore(OperatorLetter)) ~ SP).named("OperatorName")) ~> Name.Ident
    }

    /**
      * Namespaces are lower or uppercase.
      */
    def Namespace: Rule1[Name.NName] = rule {
      SP ~ oneOrMore(atomic(UpperCaseName.named("NamespaceName"))).separatedBy("/") ~ SP ~>
        ((sp1: SourcePosition, parts: Seq[Name.Ident], sp2: SourcePosition) => Name.NName(sp1, parts.toList, sp2))
    }

    def Annotation: Rule1[Name.Ident] = rule {
      atomic((LowerCaseName | UpperCaseName).named("Annotation"))
    }

    def Attribute: Rule1[Name.Ident] = namedRule("AttributeName")(atomic(LowerCaseName.named("AttributeName")))

    def Class: Rule1[Name.Ident] = namedRule("ClassName")(atomic(UpperCaseName.named("ClassName")))

    def QualifiedClass: Rule1[Name.QName] = namedRule("QualifiedClassName")(atomic(UpperCaseQName.named("QualifiedClassName")))

    def Definition: Rule1[Name.Ident] = namedRule("DefinitionName") {
      atomic((LowerCaseName | GreekName | MathName | OperatorName).named("DefinitionName"))
    }

    def QualifiedDefinition: Rule1[Name.QName] = namedRule("QualifiedDefinitionName")(atomic(LowerCaseQName.named("QualifiedDefinitionName")))

    def Eff: Rule1[Name.Ident] = namedRule("EffectName")(atomic(LowerCaseName.named("EffectName")))

    def Field: Rule1[Name.Ident] = namedRule("FieldName")(atomic(LowerCaseName.named("FieldName")))

    def Hole: Rule1[Name.Ident] = namedRule("HoleName")(atomic(LowerCaseName.named("HoleName")))

    def Intrinsic: Rule1[Name.Ident] = namedRule("IntrinsicName")(atomic(UpperCaseName.named("IntrinsicName")))

    def Predicate: Rule1[Name.Ident] = namedRule("PredicateName")(atomic(UpperCaseName.named("PredicateName")))

    def QualifiedPredicate: Rule1[Name.QName] = namedRule("QualifiedPredicateName")(atomic(UpperCaseQName.named("QualifiedPredicateName")))

    def Tag: Rule1[Name.Ident] = namedRule("TagName")(atomic(UpperCaseName.named("TagName")))

    def QualifiedTag: Rule1[Name.QName] = namedRule("QualifiedTagName")(atomic(UpperCaseQName.named("QualifiedTagName")))

    def Type: Rule1[Name.Ident] = namedRule("Type")(atomic(UpperCaseName.named("Type")))

    def QualifiedType: Rule1[Name.QName] = namedRule("QualifiedType")(atomic(UpperCaseQName.named("QualifiedType")))

    def Variable: Rule1[Name.Ident] = namedRule("VariableName") {
      atomic((LowerCaseName | Wildcard | GreekName | MathName).named("VariableName"))
    }

    def JavaIdentifier: Rule1[String] = rule {
      atomic(capture((CharPredicate.Alpha | anyOf("_$")) ~ zeroOrMore(CharPredicate.AlphaNum | anyOf("_$"))).named("JavaIdentifier"))
    }

    def JavaName: Rule1[Seq[String]] = rule {
        atomic(oneOrMore(JavaIdentifier).separatedBy(".").named("JavaName"))
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Keyword                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Reads the keyword and looks ahead to ensure there no legal letters immediately following.
    */
  def keyword(word: String): Rule0 = namedRule(s"${'"'}$word${'"'}") {
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
  def WS: Rule0 = namedRule("Whitespace") {
    quiet(atomic((oneOrMore(" " | "\t" | NewLine | Comment)).named("Whitespace")))
  }

  def optWS: Rule0 = namedRule("OptionalWhitespace") {
    quiet((optional(WS)).named("OptionalWhitespace"))
  }

  def NewLine: Rule0 = rule {
    quiet(atomic(("\n" | "\r").named("NewLine")))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Documentation                                                           //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Optionally a parses a documentation comment.
    */
  def Documentation: Rule1[ParsedAst.Doc] = {
    // Matches real whitespace.
    def PureWS: Rule0 = namedRule("Whitespace") {
      quiet(atomic(zeroOrMore(" " | "\t" | NewLine).named("Whitespace")))
    }

    // Matches triple dashed comments.
    def TripleSlashComment: Rule1[Seq[String]] = rule {
      oneOrMore(PureWS ~ atomic("///") ~ atomic((capture(zeroOrMore(!NewLine ~ ANY)) ~ (NewLine | EOI)).named("text")))
    }

    // Optionally matches a triple dashed comment and then any whitespace.
    namedRule("Documentation") {
      atomic((SP ~ optional(TripleSlashComment) ~ SP ~ optWS).named("Documentation")) ~> (
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
    atomic((Comments.SingleLineComment | Comments.MultiLineComment).named("Comment"))
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
    namedRule("SourcePosition") {
      push(SourcePosition(source, lineNumber, columnNumber, Some(input)))
    }
  }

}
