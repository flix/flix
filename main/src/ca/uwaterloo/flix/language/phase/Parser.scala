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
import ca.uwaterloo.flix.language.ast.Ast.{Source, SyntacticContext}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}
import org.parboiled2._

import scala.annotation.tailrec

/**
  * A phase to transform source files into abstract syntax trees.
  */
object Parser {

  /**
    * Parses the given source inputs into an abstract syntax tree.
    */
  def run(root: ReadAst.Root, entryPoint: Option[Symbol.DefnSym], oldRoot: ParsedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[ParsedAst.Root, CompilationMessage] =
    flix.phase("Parser") {

      if (flix.options.xparser) {
        // TODO: LEXER / PARSER2
        val res = flatMapN(Lexer.run(root))(tokens => Parser2.run(tokens))
        res match {
          case Validation.Success(tree) =>
            println(tree.values)
          case Validation.SoftFailure(_, errors) =>
            errors.foreach(e => println(e.message(flix.getFormatter)))
          case Validation.Failure(errors) =>
            errors.foreach(e => println(e.message(flix.getFormatter)))
        }
      }

      // Compute the stale and fresh sources.
      val (stale, fresh) = changeSet.partition(root.sources, oldRoot.units)

      // Parse each stale source in parallel.
      val results = ParOps.parMap(stale.keys)(parseRoot)

      // Sequence and combine the ASTs into one abstract syntax tree.
      Validation.sequence(results) map {
        case as =>
          val m = as.foldLeft(fresh) {
            case (acc, (src, u)) => acc + (src -> u)
          }
          ParsedAst.Root(m, entryPoint, root.names)
      }
    }

  /**
    * Replaces `"\n"` `"\r"` `"\t"` with spaces (not actual newlines etc. but dash n etc.).
    * If the user forgets `;` and instead the parser reads a newline and `def` then the listed
    * input is `Invalid input "\r\n\tdef"` which is replaced with `"   def"` by this method.
    */
  private def stripLiteralWhitespaceChars(s: String): String =
    s.replaceAll("\\\\n|\\\\r|\\\\t", " ")

  /**
    * Attempts to parse the given `source` as a root.
    */
  private def parseRoot(source: Source)(implicit flix: Flix): Validation[(Ast.Source, ParsedAst.CompilationUnit), CompilationMessage] = {
    flix.subtask(source.name)

    val parser = new Parser(source)
    parser.Root.run() match {
      case scala.util.Success(ast) =>
        (source, ast).toSuccess
      case scala.util.Failure(e: org.parboiled2.ParseError) =>
        val possibleContexts = parseTraces(e.traces).filter(_._1 != SyntacticContext.Unknown)
        val mostLikelyContext = possibleContexts.keySet.reduceOption(SyntacticContext.join).getOrElse(SyntacticContext.Unknown)
        val loc = SourceLocation(None, source, SourceKind.Real, e.position.line, e.position.column, e.position.line, e.position.column)
        ca.uwaterloo.flix.language.errors.ParseError(stripLiteralWhitespaceChars(parser.formatError(e)), mostLikelyContext, loc).toFailure
      case scala.util.Failure(e) =>
        ca.uwaterloo.flix.language.errors.ParseError(e.getMessage, SyntacticContext.Unknown, SourceLocation.Unknown).toFailure
    }
  }

  /**
    * Computes the nearest syntactic contexts from the given traces.
    */
  private def parseTraces(traces: Seq[RuleTrace]): Map[SyntacticContext, Int] =
    traces.map(trace => parseRuleTrace(trace.prefix.reverse)).foldLeft(Map.empty[SyntacticContext, Int]) {
      case (macc, ctx) => macc.updated(ctx, macc.getOrElse(ctx, 0) + 1)
    }

  /**
    * Computes the nearest syntactic context from the given list of non-terminals.
    */
  @tailrec
  private def parseRuleTrace(trace: List[RuleTrace.NonTerminal]): SyntacticContext = trace match {
    case Nil => SyntacticContext.Unknown
    case RuleTrace.NonTerminal(key, _) :: rest => key match {
      case RuleTrace.Named(name) =>
        // Case 1: We have a named rule application. Determine if we know it.
        syntacticContextOf(name) match {
          case SyntacticContext.Unknown =>
            // println(name)
            // Case 1.1: The named rule is not one of the contexts. Continue recursively.
            parseRuleTrace(rest)
          case result =>
            // Case 2.2: We have found a named rule we know. Return it.
            result
        }
      case _ =>
        // Case 2: We have non-named rule application. Continue recursively.
        parseRuleTrace(rest)
    }
  }

  /**
    * Returns the syntactic context of the given `name`.
    *
    * Returns [[SyntacticContext.Unknown]] if the context cannot be determined.
    */
  private def syntacticContextOf(name: String): SyntacticContext = {
    name match {
      case "Expression" => SyntacticContext.Expr.OtherExpr
      case "Constraint" => SyntacticContext.Expr.Constraint
      case "Do" => SyntacticContext.Expr.Do

      case "Class" => SyntacticContext.Decl.Class
      case "Enum" => SyntacticContext.Decl.Enum
      case "Instance" => SyntacticContext.Decl.Instance
      case "Decls" => SyntacticContext.Decl.OtherDecl

      case "Pattern" => SyntacticContext.Pat.OtherPat

      case "ImportOne" => SyntacticContext.Import
      case "ImportMany" => SyntacticContext.Import
      case "Constructor" => SyntacticContext.Import
      case "Method" => SyntacticContext.Import
      case "StaticMethod" => SyntacticContext.Import
      case "GetField" => SyntacticContext.Import
      case "PutField" => SyntacticContext.Import
      case "GetStaticField" => SyntacticContext.Import
      case "PutStaticField" => SyntacticContext.Import

      case "UseOne" => SyntacticContext.Use
      case "UseMany" => SyntacticContext.Use
      case "UseName" => SyntacticContext.Use

      case "EffectSetOrEmpty" => SyntacticContext.Type.Eff

      case "Type" => SyntacticContext.Type.OtherType

      case "WithClause" => SyntacticContext.WithClause

      case _ => SyntacticContext.Unknown
    }
  }

  object Letters {
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
    val OperatorLetter: CharPredicate = CharPredicate("+-*<>=!&|^$")

    /**
      * a (upper/lower case) letter, numeral, greek letter, or other legal character.
      */
    val LegalLetter: CharPredicate = CharPredicate.AlphaNum ++ "_" ++ "!"

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
  def Root: Rule1[ParsedAst.CompilationUnit] = rule {
    SP ~ UsesOrImports ~ Decls ~ SP ~ optWS ~ EOI ~> ParsedAst.CompilationUnit
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations                                                            //
  /////////////////////////////////////////////////////////////////////////////
  def Declaration: Rule1[ParsedAst.Declaration] = rule {
    Declarations.Namespace |
      Declarations.Def |
      Declarations.Law |
      Declarations.Enum |
      Declarations.RestrictableEnum |
      Declarations.TypeAlias |
      Declarations.Class |
      Declarations.Instance |
      Declarations.Effect
  }

  def DeclarationEOI: Rule1[ParsedAst.Declaration] = rule {
    Declaration ~ EOI
  }

  def UsesOrImports: Rule1[Seq[ParsedAst.UseOrImport]] = rule {
    // It is important for documentation comments that whitespace is not consumed if no uses are present
    oneOrMore(optWS ~ (Use | Import) ~ ((optWS ~ ";") | WS)) | push(Seq.empty)
  }

  def Decls: Rule1[Seq[ParsedAst.Declaration]] = rule {
    zeroOrMore(Declaration)
  }

  object Declarations {

    def Namespace: Rule1[ParsedAst.Declaration.Namespace] = rule {
      optWS ~ SP ~ keyword("mod") ~ WS ~ Names.DotSeparated ~ optWS ~ '{' ~ UsesOrImports ~ Decls ~ optWS ~ '}' ~ SP ~> ParsedAst.Declaration.Namespace
    }

    def Def: Rule1[ParsedAst.Declaration.Def] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ optWS ~ WithClause ~ optWS ~ OptEqualityConstraintList ~ optWS ~ "=" ~ optWS ~ Expressions.Stm ~ SP ~> ParsedAst.Declaration.Def
    }

    def Sig: Rule1[ParsedAst.Declaration.Sig] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ optWS ~ WithClause ~ optional(optWS ~ "=" ~ optWS ~ Expressions.Stm) ~ SP ~> ParsedAst.Declaration.Sig
    }

    def Law: Rule1[ParsedAst.Declaration.Law] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("law") ~ WS ~ Names.Definition ~ optWS ~ ":" ~ optWS ~ keyword("forall") ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ optWS ~ WithClause ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Declaration.Law
    }

    def Op: Rule1[ParsedAst.Declaration.Op] = rule {
      Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("def") ~ WS ~ Names.Definition ~ optWS ~ TypeParams ~ optWS ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ optWS ~ WithClause ~ SP ~> ParsedAst.Declaration.Op
    }

    def Enum: Rule1[ParsedAst.Declaration] = {
      def Case: Rule1[ParsedAst.Case] = rule {
        SP ~ Names.Tag ~ optional(Types.Tuple) ~ SP ~> ParsedAst.Case
      }

      def CaseList: Rule1[Seq[ParsedAst.Case]] = rule {
        NonEmptyCaseList | push(Nil)
      }

      def NonEmptyCaseList: Rule1[Seq[ParsedAst.Case]] = rule {
        // Note: We use the case keyword as part of the separator with or without a comma.
        keyword("case") ~ WS ~ oneOrMore(Case).separatedBy(
          (optWS ~ "," ~ optWS ~ keyword("case") ~ WS) | (WS ~ keyword("case") ~ WS) | (optWS ~ "," ~ optWS)
        )
      }

      def Body = namedRule("CaseBody") {
        optional(optWS ~ "{" ~ optWS ~ CaseList ~ optWS ~ "}")
      }

      rule {
        Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("enum") ~ WS ~ Names.Type ~ TypeParams ~ optional(Types.Tuple) ~ Derivations ~ optWS ~ Body ~ SP ~> ParsedAst.Declaration.Enum
      }
    }

    def RestrictableEnum: Rule1[ParsedAst.Declaration] = {
      def Case: Rule1[ParsedAst.RestrictableCase] = rule {
        SP ~ Names.Tag ~ optional(Types.Tuple) ~ SP ~> ParsedAst.RestrictableCase
      }

      def CaseList: Rule1[Seq[ParsedAst.RestrictableCase]] = rule {
        NonEmptyCaseList | push(Nil)
      }

      def NonEmptyCaseList: Rule1[Seq[ParsedAst.RestrictableCase]] = rule {
        // Note: We use the case keyword as part of the separator with or without a comma.
        keyword("case") ~ WS ~ oneOrMore(Case).separatedBy(
          (optWS ~ "," ~ optWS ~ keyword("case") ~ WS) | (WS ~ keyword("case") ~ WS) | (optWS ~ "," ~ optWS)
        )
      }

      def Body = namedRule("CaseBody") {
        optional(optWS ~ "{" ~ optWS ~ CaseList ~ optWS ~ "}")
      }

      def RestrictionParameter: Rule1[ParsedAst.TypeParam] = rule {
        "[" ~ optWS ~ SP ~ Names.Variable ~ push(None: Option[ParsedAst.Kind]) ~ SP ~ optWS ~ "]" ~> ParsedAst.TypeParam
      }

      rule {
        Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("restrictable") ~ WS ~ keyword("enum") ~ WS ~ Names.Type ~ RestrictionParameter ~ TypeParams ~ optional(Types.Tuple) ~ Derivations ~ optWS ~ Body ~ SP ~> ParsedAst.Declaration.RestrictableEnum
      }
    }

    def TypeAlias: Rule1[ParsedAst.Declaration.TypeAlias] = rule {
      Documentation ~ Modifiers ~ SP ~ keyword("type") ~ WS ~ keyword("alias") ~ WS ~ Names.Type ~ optWS ~ TypeParams ~ optWS ~ "=" ~ optWS ~ Type ~ SP ~> ParsedAst.Declaration.TypeAlias
    }

    def AssocTypeSig: Rule1[ParsedAst.Declaration.AssocTypeSig] = rule {
      Documentation ~ Modifiers ~ SP ~ keyword("type") ~ WS ~ Names.Type ~ optWS ~ TypeParams ~ optWS ~ optional(":" ~ optWS ~ Kind) ~ SP ~> ParsedAst.Declaration.AssocTypeSig
    }

    def AssocTypeDef: Rule1[ParsedAst.Declaration.AssocTypeDef] = rule {
      Documentation ~ Modifiers ~ SP ~ keyword("type") ~ WS ~ Names.Type ~ optWS ~ optional("[" ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ "]") ~ optWS ~ "=" ~ optWS ~ Type ~ SP ~> ParsedAst.Declaration.AssocTypeDef
    }

    def Class: Rule1[ParsedAst.Declaration] = {
      def Head = rule {
        Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("class") ~ WS ~ Names.Class ~ optWS ~ "[" ~ optWS ~ TypeParam ~ optWS ~ "]" ~ optWS ~ WithClause
      }

      def EmptyBody = namedRule("ClassBody") {
        push(Nil) ~ push(Nil) ~ SP
      }

      def NonEmptyBody = namedRule("ClassBody") {
        optWS ~ "{" ~ optWS ~ zeroOrMore(Declarations.AssocTypeSig) ~ zeroOrMore(Declarations.Law | Declarations.Sig) ~ optWS ~ "}" ~ SP
      }

      rule {
        Head ~ (NonEmptyBody | EmptyBody) ~> ParsedAst.Declaration.Class
      }
    }

    def TypeConstraint: Rule1[ParsedAst.TypeConstraint] = rule {
      SP ~ Names.QualifiedClass ~ optWS ~ "[" ~ optWS ~ Type ~ optWS ~ "]" ~ SP ~> ParsedAst.TypeConstraint
    }

    def WithClause: Rule1[Seq[ParsedAst.TypeConstraint]] = rule {
      optional(keyword("with") ~ WS ~ oneOrMore(TypeConstraint).separatedBy(optWS ~ "," ~ optWS)) ~> ((o: Option[Seq[ParsedAst.TypeConstraint]]) => o.getOrElse(Seq.empty))
    }

    def EqualityConstraint: Rule1[ParsedAst.EqualityConstraint] = rule {
      SP ~ Type ~ optWS ~ "~" ~ optWS ~ Type ~ SP ~> ParsedAst.EqualityConstraint
    }

    def OptEqualityConstraintList: Rule1[Seq[ParsedAst.EqualityConstraint]] = rule {
      optional(keyword("where") ~ WS ~ oneOrMore(EqualityConstraint).separatedBy(optWS ~ "," ~ optWS)) ~> ((o: Option[Seq[ParsedAst.EqualityConstraint]]) => o.getOrElse(Seq.empty))
    }

    def Instance: Rule1[ParsedAst.Declaration] = {
      def Head = rule {
        Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("instance") ~ WS ~ Names.QualifiedClass ~ optWS ~ "[" ~ optWS ~ Type ~ optWS ~ "]" ~ optWS ~ WithClause
      }

      def EmptyBody = namedRule("InstanceBody") {
        push(Nil) ~ push(Nil) ~ SP
      }

      def NonEmptyBody = namedRule("InstanceBody") {
        optWS ~ "{" ~ optWS ~ zeroOrMore(Declarations.AssocTypeDef) ~ zeroOrMore(Declarations.Def) ~ optWS ~ "}" ~ SP
      }

      rule {
        Head ~ (NonEmptyBody | EmptyBody) ~> ParsedAst.Declaration.Instance
      }
    }

    def Effect: Rule1[ParsedAst.Declaration] = {
      def Head = rule {
        Documentation ~ Annotations ~ Modifiers ~ SP ~ keyword("eff") ~ WS ~ Names.Effect ~ optWS ~ TypeParams
      }

      def EmptyBody = namedRule("EffectBody") {
        push(Nil) ~ SP
      }

      def NonEmptyBody = namedRule("EffectBody") {
        optWS ~ "{" ~ optWS ~ zeroOrMore(Declarations.Op) ~ optWS ~ "}" ~ SP
      }

      rule {
        Head ~ (NonEmptyBody | EmptyBody) ~> ParsedAst.Declaration.Effect
      }
    }

    def TypeParam: Rule1[ParsedAst.TypeParam] = rule {
      SP ~ Names.Variable ~ optional(optWS ~ ":" ~ optWS ~ Kind) ~ SP ~> ParsedAst.TypeParam
    }

    def TypeParams: Rule1[ParsedAst.TypeParams] = {
      rule {
        optional("[" ~ optWS ~ oneOrMore(TypeParam).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]") ~> ((o: Option[Seq[ParsedAst.TypeParam]]) => o match {
          case None => ParsedAst.TypeParams.Elided
          case Some(xs) => ParsedAst.TypeParams.Explicit(xs.toList)
        })
      }
    }

    def Derivations: Rule1[ParsedAst.Derivations] = {
      def WithClause: Rule1[Seq[Name.QName]] = rule {
        keyword("with") ~ WS ~ oneOrMore(Names.QualifiedClass).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        ((optWS ~ SP ~ WithClause ~ SP) | (SP ~ push(Seq.empty[Name.QName]) ~ SP)) ~> ParsedAst.Derivations
      }
    }

  }

  def Attribute: Rule1[ParsedAst.Attribute] = rule {
    SP ~ Names.Attribute ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Attribute
  }

  def TypeAndEffect: Rule2[ParsedAst.Type, Option[ParsedAst.Type]] = {

    def EmptyEffectSet: Rule1[ParsedAst.Type] = rule {
      SP ~ "{" ~ optWS ~ push(Nil) ~ "}" ~ SP ~> ParsedAst.Type.EffectSet
    }

    // First tries to parse the type as an empty effect set, so that {} is interpreted as a set rather than a record
    def EffectFirstType: Rule1[ParsedAst.Type] = rule {
      EmptyEffectSet | Type
    }

    rule {
      Type ~ optional(optWS ~ "\\" ~ optWS ~ EffectFirstType)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Uses                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  def Use: Rule1[ParsedAst.Use] = rule {
    keyword("use") ~ WS ~ (Uses.UseMany | Uses.UseOne)
  }

  object Uses {
    def UseOne: Rule1[ParsedAst.Use.UseOne] = rule {
      SP ~ Names.QName ~ SP ~> ParsedAst.Use.UseOne
    }

    def UseMany: Rule1[ParsedAst.Use.UseMany] = {
      def NameAndAlias: Rule1[ParsedAst.Use.NameAndAlias] = rule {
        SP ~ UseName ~ optional(WS ~ atomic("=>") ~ WS ~ UseName) ~ SP ~> ParsedAst.Use.NameAndAlias
      }

      rule {
        SP ~ Names.DotSeparated ~ atomic(".{") ~ optWS ~ zeroOrMore(NameAndAlias).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Use.UseMany
      }
    }

    def UseName: Rule1[Name.Ident] = rule {
      Names.LowerCaseName | Names.UpperCaseName | Names.GreekName | Names.MathName | Names.OperatorName
    }
  }

  def Import: Rule1[ParsedAst.Import] = rule {
    keyword("import") ~ WS ~ (Imports.ImportMany | Imports.ImportOne)
  }

  object Imports {
    def ImportOne: Rule1[ParsedAst.Imports.ImportOne] = rule {
      SP ~ Names.JavaName ~ SP ~> ParsedAst.Imports.ImportOne
    }

    def ImportMany: Rule1[ParsedAst.Imports.ImportMany] = {
      def NameAndAlias: Rule1[ParsedAst.Imports.NameAndAlias] = rule {
        SP ~ Names.JavaIdentifier ~ optional(WS ~ atomic("=>") ~ WS ~ Names.UpperCaseName) ~ SP ~> ParsedAst.Imports.NameAndAlias
      }

      rule {
        SP ~ Names.JavaName ~ atomic(".{") ~ optWS ~ zeroOrMore(NameAndAlias).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Imports.ImportMany
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    Literals.Null | Literals.Bool | Literals.Char | Literals.Str | Literals.Float | Literals.Int | Literals.Regex
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
      SP ~ "'" ~ zeroOrMore(!"'" ~ Chars.CharCode) ~ "'" ~ SP ~> ParsedAst.Literal.Char
    }

    // Note that outside of patterns, Strings are parsed as [[Interpolation]]s
    def Str: Rule1[ParsedAst.Literal.Str] = rule {
      SP ~ "\"" ~ zeroOrMore(!("\"" | atomic("${") | atomic("%{")) ~ Chars.CharCode) ~ "\"" ~ SP ~> ParsedAst.Literal.Str
    }

    def Regex: Rule1[ParsedAst.Literal.Regex] = rule {
      SP ~ "regex\"" ~ zeroOrMore(!("\"") ~ Chars.CharCode) ~ "\"" ~ SP ~> ParsedAst.Literal.Regex
    }

    def Float: Rule1[ParsedAst.Literal] = rule {
      Float32 | Float64 | BigDecimal | FloatDefault
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

    def BigDecimal: Rule1[ParsedAst.Literal.BigDecimal] = rule {
      SP ~ Sign ~ SeparableDecDigits ~ optional("." ~ SeparableDecDigits) ~ optional(("e" | "E") ~ SeparableDecDigits) ~ atomic("ff") ~ SP ~> ParsedAst.Literal.BigDecimal
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

    def Sign: Rule1[String] = rule {
      capture("-" | "+" | "")
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

    def CharCode: Rule1[ParsedAst.CharCode] = rule {
      Escape | Literal
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  def Expression: Rule1[ParsedAst.Expression] = rule {
    Expressions.Assign
  }

  def ExpressionEOI: Rule1[ParsedAst.Expression] = rule {
    Expression ~ EOI
  }

  object Expressions {
    def Stm: Rule1[ParsedAst.Expression] = rule {
      Expression ~ optional(optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.Stm)
    }

    def GuardFragment: Rule1[ParsedAst.ForFragment.Guard] = rule {
      SP ~ keyword("if") ~ WS ~ Expression ~ SP ~> ParsedAst.ForFragment.Guard
    }

    def GeneratorFragment: Rule1[ParsedAst.ForFragment.Generator] = rule {
      SP ~ Pattern ~ WS ~ keyword("<-") ~ WS ~ Expression ~ SP ~> ParsedAst.ForFragment.Generator
    }

    def ForFragment: Rule1[ParsedAst.ForFragment] = {
      rule {
        GeneratorFragment | GuardFragment
      }
    }

    def ForFragments: Rule1[Seq[ParsedAst.ForFragment]] = rule {
      "(" ~ optWS ~ zeroOrMore(ForFragment).separatedBy(optWS ~ ";" ~ optWS) ~ optWS ~ ")"
    }

    def ApplicativeFor: Rule1[ParsedAst.Expression.ApplicativeFor] = rule {
      SP ~ keyword("forA") ~ optWS ~ "(" ~ optWS ~ zeroOrMore(GeneratorFragment).separatedBy(optWS ~ ";" ~ optWS) ~ optWS ~ ")" ~ optWS ~ keyword("yield") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.ApplicativeFor
    }

    def ForEach: Rule1[ParsedAst.Expression.ForEach] = rule {
      SP ~ keyword("foreach") ~ optWS ~ ForFragments ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.ForEach
    }

    def MonadicFor: Rule1[ParsedAst.Expression.MonadicFor] = rule {
      SP ~ (keyword("forM") | keyword("for")) ~ optWS ~ ForFragments ~ optWS ~ keyword("yield") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.MonadicFor
    }

    def ForEachYield: Rule1[ParsedAst.Expression.ForEachYield] = rule {
      SP ~ keyword("foreach") ~ optWS ~ ForFragments ~ optWS ~ keyword("yield") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.ForEachYield
    }

    def Assign: Rule1[ParsedAst.Expression] = rule {
      InstanceOf ~ optional(optWS ~ operatorX(":=") ~ optWS ~ LogicalOr ~ SP ~> ParsedAst.Expression.Assign)
    }

    def InstanceOf: Rule1[ParsedAst.Expression] = rule {
      LogicalOr ~ optional(WS ~ operatorX("instanceof") ~ WS ~ atomic("##") ~ Names.JavaName ~ SP ~> ParsedAst.Expression.InstanceOf)
    }

    def LogicalOr: Rule1[ParsedAst.Expression] = {
      def Or: Rule1[ParsedAst.Operator] = rule {
        WS ~ operator("or") ~ WS
      }

      rule {
        LogicalAnd ~ zeroOrMore(Or ~ LogicalAnd ~ SP ~> ParsedAst.Expression.Binary)
      }
    }

    def LogicalAnd: Rule1[ParsedAst.Expression] = {
      def And: Rule1[ParsedAst.Operator] = rule {
        WS ~ operator("and") ~ WS
      }

      rule {
        Equality ~ zeroOrMore(And ~ Equality ~ SP ~> ParsedAst.Expression.Binary)
      }
    }

    def Equality: Rule1[ParsedAst.Expression] = rule {
      // NB: use optional here to prevent (x == y == z)
      Relational ~ optional(optWS ~ (operator("==") | operator("!=") | operator("<=>")) ~ optWS ~ Relational ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Relational: Rule1[ParsedAst.Expression] = rule {
      // NB: use optional here to prevent (x <= y <= z)
      Additive ~ optional(WS ~ (operator("<=") | operator(">=") | operator("<") | operator(">")) ~ WS ~ Additive ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Additive: Rule1[ParsedAst.Expression] = rule {
      Multiplicative ~ zeroOrMore(optWS ~ (operator("+") | operator("-")) ~ optWS ~ Multiplicative ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Multiplicative: Rule1[ParsedAst.Expression] = rule {
      Compose ~ zeroOrMore(optWS ~ (operator("*") | operator("/")) ~ optWS ~ Compose ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Compose: Rule1[ParsedAst.Expression] = rule {
      Infix ~ zeroOrMore(optWS ~ operatorX("<+>") ~ optWS ~ Infix ~ SP ~> ParsedAst.Expression.FixpointCompose)
    }

    def Infix: Rule1[ParsedAst.Expression] = rule {
      Special ~ zeroOrMore(optWS ~ "`" ~ QName ~ "`" ~ optWS ~ Special ~ SP ~> ParsedAst.Expression.Infix)
    }

    def Special: Rule1[ParsedAst.Expression] = {
      import Parser.Letters

      // NB: We allow any operator, other than a reserved operator, to be matched by this rule.
      def Reserved2: Rule1[String] = rule {
        capture("!=" | "::" | ":=" | "<-" | "<=" | "==" | "=>" | ">=" | "or")
      }

      // NB: We allow any operator, other than a reserved operator, to be matched by this rule.
      def Reserved3: Rule1[String] = rule {
        capture(":::" | "<+>" | "<=>" | "???" | "and" | "not")
      }

      // Match any two character operator which is not reserved.
      def UserOp2: Rule1[String] = rule {
        !Reserved2 ~ capture(Letters.OperatorLetter ~ Letters.OperatorLetter)
      }

      // Match any three character operator which is not reserved.
      def UserOp3: Rule1[String] = rule {
        !Reserved3 ~ capture(Letters.OperatorLetter ~ Letters.OperatorLetter ~ Letters.OperatorLetter)
      }

      // Match any operator which has at least four characters.
      def UserOpN: Rule1[String] = rule {
        capture(Letters.OperatorLetter ~ Letters.OperatorLetter ~ Letters.OperatorLetter ~ oneOrMore(Letters.OperatorLetter))
      }

      // Match any mathematical operator or symbol.
      def MathOp: Rule1[String] = rule {
        capture(Letters.MathLetter)
      }

      // Capture the source positions around the operator.
      // NB: UserOpN must occur before UserOp2.
      def Op: Rule1[ParsedAst.Operator] = rule {
        SP ~ (UserOpN | UserOp3 | UserOp2 | MathOp) ~ SP ~> ParsedAst.Operator
      }

      rule {
        Unary ~ zeroOrMore(optWS ~ Op ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Binary)
      }
    }

    def Unary: Rule1[ParsedAst.Expression] = {
      def UnaryOp1: Rule1[ParsedAst.Operator] = rule {
        operator("-")
      }

      def UnaryOp2: Rule1[ParsedAst.Operator] = rule {
        operator("not") ~ WS
      }

      rule {
        !Literal ~ (SP ~ (UnaryOp1 | UnaryOp2) ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Unary) | Ref
      }
    }

    // TODO: Why are these not primary?
    def Ref: Rule1[ParsedAst.Expression] = rule {
      (SP ~ keyword("ref") ~ WS ~ Ref ~ WS ~ keyword("@") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Ref) | Deref
    }

    def Deref: Rule1[ParsedAst.Expression] = rule {
      (SP ~ keyword("deref") ~ WS ~ Deref ~ SP ~> ParsedAst.Expression.Deref) | Without
    }

    def Without: Rule1[ParsedAst.Expression] = {
      def Effects: Rule1[Seq[Name.QName]] = rule {
        Names.QualifiedEffect ~> ((n: Name.QName) => List(n)) | "{" ~ optWS ~ oneOrMore(Names.QualifiedEffect).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}"
      }

      rule {
        Ascribe ~ optional(WS ~ keyword("without") ~ WS ~ Effects ~ SP ~> ParsedAst.Expression.Without)
      }
    }

    def Ascribe: Rule1[ParsedAst.Expression] = rule {
      FAppend ~ optional(optWS ~ ":" ~ optWS ~ TypeAndEffect ~ SP ~> ParsedAst.Expression.Ascribe)
    }

    def Primary: Rule1[ParsedAst.Expression] = rule {
      Static | Scope | LetMatch | LetRecDef | LetUse | LetImport | IfThenElse |
        RelationalChoose | RestrictableChoose | TypeMatch | Match | LambdaMatch | Try | Lambda | Tuple |
        RecordOperation | RecordLiteral | Block |
        SelectChannel | Spawn | ParYield | Lazy | Force |
        CheckedTypeCast | CheckedEffectCast | UncheckedCast | UncheckedMaskingCast | Intrinsic | ArrayLit | VectorLit | ListLit |
        SetLit | FMap | ConstraintSet | FixpointLambda | FixpointProject | FixpointSolveWithProject |
        FixpointQueryWithSelect | Interpolation | Literal | Resume | Do |
        Discard | Debug | ApplicativeFor | ForEachYield | MonadicFor | ForEach | NewObject |
        UnaryLambda | Open | OpenAs | HolyName | QName | Hole
    }

    def CheckedTypeCast: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("checked_cast") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.CheckedTypeCast
    }

    def CheckedEffectCast: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("checked_ecast") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.CheckedEffectCast
    }

    def UncheckedCast: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("unchecked_cast") ~ optWS ~ "(" ~ Expression ~ WS ~ "as" ~ optWS ~ TypeAndEffect ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.UncheckedCast
    }

    def UncheckedMaskingCast: Rule1[ParsedAst.Expression.UncheckedMaskingCast] = rule {
      SP ~ keyword("masked_cast") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.UncheckedMaskingCast
    }

    def Literal: Rule1[ParsedAst.Expression.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Expression.Lit
    }

    def Interpolation: Rule1[ParsedAst.Expression.Interpolation] = {
      def ExpPart: Rule1[ParsedAst.InterpolationPart] = rule {
        SP ~ atomic("${") ~ optWS ~ optional(Expression) ~ optWS ~ "}" ~ SP ~> ParsedAst.InterpolationPart.ExpPart
      }

      def DebugPart: Rule1[ParsedAst.InterpolationPart] = rule {
        SP ~ atomic("%{") ~ optWS ~ optional(Expression) ~ optWS ~ "}" ~ SP ~> ParsedAst.InterpolationPart.DebugPart
      }

      def StrPart: Rule1[ParsedAst.InterpolationPart] = rule {
        SP ~ oneOrMore(!("\"" | atomic("${") | atomic("%{")) ~ Chars.CharCode) ~ SP ~> ParsedAst.InterpolationPart.StrPart
      }

      def InterpolationPart: Rule1[ParsedAst.InterpolationPart] = rule {
        ExpPart | DebugPart | StrPart
      }

      rule {
        SP ~ "\"" ~ zeroOrMore(InterpolationPart) ~ "\"" ~ SP ~> ParsedAst.Expression.Interpolation
      }
    }

    def IfThenElse: Rule1[ParsedAst.Expression.IfThenElse] = rule {
      SP ~ keyword("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ WS ~ keyword("else") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
    }

    def LetMatch: Rule1[ParsedAst.Expression.LetMatch] = rule {
      SP ~ keyword("let") ~ WS ~ Modifiers ~ Pattern ~ optWS ~ optional(":" ~ optWS ~ Type ~ optWS) ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetMatch
    }

    def LetRecDef: Rule1[ParsedAst.Expression.LetRecDef] = {
      def SomeTypeAndEffect: Rule1[Option[(ParsedAst.Type, Option[ParsedAst.Type])]] = rule {
        ":" ~ optWS ~ TypeAndEffect ~ optWS ~> ((tpe: ParsedAst.Type, eff: Option[ParsedAst.Type]) => Some((tpe, eff)))
      }

      def NoTypeAndEffect: Rule1[Option[(ParsedAst.Type, Option[ParsedAst.Type])]] = rule {
        push(None)
      }

      rule {
        SP ~ keyword("def") ~ WS ~ Names.Definition ~ optWS ~ FormalParamList ~ optWS ~ (SomeTypeAndEffect | NoTypeAndEffect) ~ "=" ~ optWS ~ Expression ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetRecDef
      }
    }

    def LetUse: Rule1[ParsedAst.Expression.Use] = rule {
      SP ~ Use ~ optWS ~ ";" ~ optWS ~ Expressions.Stm ~ SP ~> ParsedAst.Expression.Use
    }

    def Static: Rule1[ParsedAst.Expression.Static] = rule {
      SP ~ keyword("Static") ~ SP ~> ParsedAst.Expression.Static
    }

    def Scope: Rule1[ParsedAst.Expression.Scope] = rule {
      SP ~ keyword("region") ~ WS ~ Names.Variable ~ optWS ~ Expressions.Block ~ SP ~> ParsedAst.Expression.Scope
    }

    def LetImport: Rule1[ParsedAst.Expression] = {

      def Constructor: Rule1[ParsedAst.JvmOp] = rule {
        keyword("new") ~ WS ~ Names.JavaName ~ optWS ~ Signature ~ optWS ~ Ascription ~ optWS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.Constructor
      }

      def Method: Rule1[ParsedAst.JvmOp] = rule {
        Names.JavaName ~ optWS ~ Signature ~ optWS ~ Ascription ~ optional(optWS ~ keyword("as") ~ WS ~ Names.Variable) ~> ParsedAst.JvmOp.Method
      }

      def StaticMethod: Rule1[ParsedAst.JvmOp] = rule {
        keyword("static") ~ WS ~ Names.JavaName ~ optWS ~ Signature ~ optWS ~ Ascription ~ optional(optWS ~ keyword("as") ~ WS ~ Names.Variable) ~> ParsedAst.JvmOp.StaticMethod
      }

      def GetField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("get") ~ WS ~ Names.JavaName ~ optWS ~ Ascription ~ optWS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.GetField
      }

      def PutField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("set") ~ WS ~ Names.JavaName ~ optWS ~ Ascription ~ optWS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.PutField
      }

      def GetStaticField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("static") ~ WS ~ keyword("get") ~ WS ~ Names.JavaName ~ optWS ~ Ascription ~ optWS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.GetStaticField
      }

      def PutStaticField: Rule1[ParsedAst.JvmOp] = rule {
        keyword("static") ~ WS ~ keyword("set") ~ WS ~ Names.JavaName ~ optWS ~ Ascription ~ optWS ~ keyword("as") ~ WS ~ Names.Variable ~> ParsedAst.JvmOp.PutStaticField
      }

      def Signature: Rule1[Seq[ParsedAst.Type]] = rule {
        "(" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
      }

      def Ascription: Rule2[ParsedAst.Type, Option[ParsedAst.Type]] = rule {
        ":" ~ optWS ~ TypeAndEffect
      }

      def Import: Rule1[ParsedAst.JvmOp] = rule {
        keyword("import") ~ WS ~ (Constructor | Method | StaticMethod | GetField | PutField | GetStaticField | PutStaticField)
      }

      rule {
        SP ~ Import ~ optWS ~ ";" ~ optWS ~ Stm ~ SP ~> ParsedAst.Expression.LetImport
      }
    }

    def NewObject: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("new") ~ WS ~ Type ~ optWS ~ "{" ~ optWS ~ zeroOrMore(JvmMethod).separatedBy(WS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.NewObject
    }

    def JvmMethod: Rule1[ParsedAst.JvmMethod] = rule {
      SP ~ keyword("def") ~ WS ~ Names.JavaMethod ~ optWS ~ FormalParamList ~ optWS ~ ":" ~ optWS ~ TypeAndEffect ~ optWS ~ "=" ~ optWS ~ Expressions.Stm ~ SP ~> ParsedAst.JvmMethod
    }

    def Match: Rule1[ParsedAst.Expression.Match] = {
      def Rule: Rule1[ParsedAst.MatchRule] = rule {
        keyword("case") ~ WS ~ Pattern ~ optWS ~ optional(keyword("if") ~ WS ~ Expression ~ optWS) ~ atomic("=>") ~ optWS ~ Stm ~> ParsedAst.MatchRule
      }

      rule {
        SP ~ keyword("match") ~ WS ~ Expression ~ optWS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(CaseSeparator) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Match
      }
    }

    def TypeMatch: Rule1[ParsedAst.Expression.TypeMatch] = {
      def Rule: Rule1[ParsedAst.TypeMatchRule] = rule {
        keyword("case") ~ WS ~ Names.Variable ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ atomic("=>") ~ optWS ~ Stm ~> ParsedAst.TypeMatchRule
      }

      rule {
        SP ~ keyword("typematch") ~ WS ~ Expression ~ optWS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(CaseSeparator) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.TypeMatch
      }
    }

    def RelationalChoose: Rule1[ParsedAst.Expression.RelationalChoose] = {
      def MatchOne: Rule1[Seq[ParsedAst.Expression]] = rule {
        Expression ~> ((e: ParsedAst.Expression) => Seq(e))
      }

      def MatchMany: Rule1[Seq[ParsedAst.Expression]] = rule {
        "(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
      }

      def ChoicePattern: Rule1[ParsedAst.RelationalChoosePattern] = rule {
        (SP ~ "_" ~ SP ~> ParsedAst.RelationalChoosePattern.Wild) |
          (SP ~ keyword("Absent") ~ SP ~> ParsedAst.RelationalChoosePattern.Absent) |
          (SP ~ keyword("Present") ~ optWS ~ "(" ~ Names.Variable ~ ")" ~ SP ~> ParsedAst.RelationalChoosePattern.Present)
      }

      def CaseOne: Rule1[ParsedAst.RelationalChooseRule] = namedRule("ChooseCase") {
        SP ~ keyword("case") ~ WS ~ ChoicePattern ~ WS ~ atomic("=>") ~ WS ~ Expression ~ SP ~>
          ((sp1: SourcePosition, x: ParsedAst.RelationalChoosePattern, e: ParsedAst.Expression, sp2: SourcePosition) => ParsedAst.RelationalChooseRule(sp1, Seq(x), e, sp2))
      }

      def CaseMany: Rule1[ParsedAst.RelationalChooseRule] = namedRule("ChooseCase") {
        SP ~ keyword("case") ~ WS ~ "(" ~ optWS ~ oneOrMore(ChoicePattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ WS ~ atomic("=>") ~ WS ~ Expression ~ SP ~> ParsedAst.RelationalChooseRule
      }

      def ChooseKind: Rule1[Boolean] = rule {
        (keyword("relational_choose*") ~ push(true)) | (keyword("relational_choose") ~ push(false))
      }

      rule {
        SP ~ ChooseKind ~ WS ~ (MatchMany | MatchOne) ~ optWS ~ "{" ~ optWS ~ oneOrMore(CaseMany | CaseOne).separatedBy(WS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.RelationalChoose
      }
    }

    def RestrictableChoose: Rule1[ParsedAst.Expression.RestrictableChoose] = {
      def Rule: Rule1[ParsedAst.MatchRule] = rule {
        keyword("case") ~ WS ~ Pattern ~ optWS ~ optional(keyword("if") ~ WS ~ Expression ~ optWS) ~ atomic("=>") ~ optWS ~ Stm ~> ParsedAst.MatchRule
      }

      def ChooseKind: Rule1[Boolean] = rule {
        (keyword("choose*") ~ push(true)) | (keyword("choose") ~ push(false))
      }

      rule {
        SP ~ ChooseKind ~ WS ~ Expression ~ optWS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(CaseSeparator) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.RestrictableChoose
      }
    }

    def Do: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("do") ~ WS ~ Names.QualifiedOperation ~ ArgumentList ~ SP ~> ParsedAst.Expression.Do
    }

    def Resume: Rule1[ParsedAst.Expression] = rule {
      SP ~ keyword("resume") ~ Argument ~ SP ~> ParsedAst.Expression.Resume
    }

    def Debug: Rule1[ParsedAst.Expression.Debug] = {
      def DebugKind: Rule1[ParsedAst.DebugKind] = rule {
        keyword("debug!!") ~ push(ParsedAst.DebugKind.DebugWithLocAndSrc) |
          keyword("debug!") ~ push(ParsedAst.DebugKind.DebugWithLoc) |
          keyword("debug") ~ push(ParsedAst.DebugKind.Debug)
      }

      rule {
        SP ~ DebugKind ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Debug
      }
    }

    def Discard: Rule1[ParsedAst.Expression.Discard] = rule {
      SP ~ keyword("discard") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Discard
    }

    def Try: Rule1[ParsedAst.Expression] = {
      def CatchRule: Rule1[ParsedAst.CatchRule] = rule {
        keyword("case") ~ WS ~ Names.Variable ~ optWS ~ ":" ~ optWS ~ atomic("##") ~ Names.JavaName ~ WS ~ atomic("=>") ~ optWS ~ Expression ~> ParsedAst.CatchRule
      }

      def CatchBody: Rule1[ParsedAst.CatchOrHandler] = rule {
        keyword("catch") ~ optWS ~ "{" ~ optWS ~ oneOrMore(CatchRule).separatedBy(CaseSeparator) ~ optWS ~ "}" ~> ParsedAst.CatchOrHandler.Catch
      }

      def HandlerRule: Rule1[ParsedAst.HandlerRule] = rule {
        keyword("def") ~ WS ~ Names.Operation ~ FormalParamList ~ optWS ~ atomic("=") ~ optWS ~ Expression ~> ParsedAst.HandlerRule
      }

      def HandlerBody: Rule1[ParsedAst.CatchOrHandler] = rule {
        keyword("with") ~ optWS ~ Names.QualifiedEffect ~ optional(optWS ~ "{" ~ optWS ~ zeroOrMore(HandlerRule).separatedBy(CaseSeparator) ~ optWS ~ "}") ~> ParsedAst.CatchOrHandler.Handler
      }

      def Body: Rule1[ParsedAst.CatchOrHandler] = rule {
        CatchBody | HandlerBody
      }

      rule {
        SP ~ keyword("try") ~ WS ~ Expression ~ optWS ~ Body ~ SP ~> ParsedAst.Expression.Try
      }
    }

    def RecordSelect: Rule1[ParsedAst.Expression] = rule {
      Apply ~ zeroOrMore(optWS ~ "." ~ Names.Field ~ SP ~> ParsedAst.Expression.RecordSelect)
    }

    def SelectChannel: Rule1[ParsedAst.Expression.SelectChannel] = {
      def SelectChannelRule: Rule1[ParsedAst.SelectChannelRule] = rule {
        keyword("case") ~ WS ~ Names.Variable ~ optWS ~ atomic("<-") ~ optWS ~ optional(atomic("Channel.")) ~ atomic("recv") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ atomic("=>") ~ optWS ~ Stm ~> ParsedAst.SelectChannelRule
      }

      def SelectChannelDefault: Rule1[ParsedAst.Expression] = rule {
        keyword("case") ~ WS ~ "_" ~ optWS ~ atomic("=>") ~ optWS ~ Stm
      }

      rule {
        SP ~ keyword("select") ~ WS ~ "{" ~ optWS ~ oneOrMore(SelectChannelRule).separatedBy(CaseSeparator) ~ optWS ~ optional(SelectChannelDefault) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.SelectChannel
      }
    }

    def Spawn: Rule1[ParsedAst.Expression.Spawn] = rule {
      SP ~ keyword("spawn") ~ WS ~ Expression ~ WS ~ keyword("@") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.Spawn
    }

    def ParYield: Rule1[ParsedAst.Expression.ParYield] = {

      def Fragment: Rule1[ParsedAst.ParYieldFragment] = rule {
        SP ~ Pattern ~ optWS ~ atomic("<-") ~ optWS ~ Expression ~ SP ~> ParsedAst.ParYieldFragment
      }

      rule {
        SP ~ keyword("par") ~ optWS ~ "(" ~ optWS ~ oneOrMore(Fragment).separatedBy(optWS ~ ";" ~ optWS) ~ optWS ~ ")" ~ optWS ~ keyword("yield") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.ParYield
      }
    }

    def Lazy: Rule1[ParsedAst.Expression.Lazy] = rule {
      SP ~ keyword("lazy") ~ WS ~ RecordSelect ~ SP ~> ParsedAst.Expression.Lazy
    }

    def Force: Rule1[ParsedAst.Expression.Force] = rule {
      SP ~ keyword("force") ~ WS ~ RecordSelect ~ SP ~> ParsedAst.Expression.Force
    }

    def Intrinsic: Rule1[ParsedAst.Expression.Intrinsic] = rule {
      SP ~ "$" ~ Names.Intrinsic ~ "$" ~ ArgumentList ~ SP ~> ParsedAst.Expression.Intrinsic
    }

    def Apply: Rule1[ParsedAst.Expression] = rule {
      Primary ~ zeroOrMore(ArgumentList ~ SP ~> ParsedAst.Expression.Apply)
    }

    def Tuple: Rule1[ParsedAst.Expression] = rule {
      SP ~ ArgumentList ~ SP ~> ParsedAst.Expression.Tuple
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
        SP ~ "{" ~ optWS ~ oneOrMore(RecordOp).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "|" ~ optWS ~ Expression ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.RecordOperation
      }
    }

    def FAppend: Rule1[ParsedAst.Expression] = rule {
      FCons ~ optional(optWS ~ SP ~ operatorX(":::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FAppend)
    }

    def FCons: Rule1[ParsedAst.Expression] = rule {
      RecordSelect ~ optional(optWS ~ SP ~ operatorX("::") ~ SP ~ optWS ~ Expression ~> ParsedAst.Expression.FCons)
    }

    def ArrayLit: Rule1[ParsedAst.Expression.ArrayLit] = rule {
      SP ~ atomic("Array") ~ atomic("#{") ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ WS ~ keyword("@") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.ArrayLit
    }

    def VectorLit: Rule1[ParsedAst.Expression.VectorLit] = rule {
      SP ~ atomic("Vector") ~ atomic("#{") ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.VectorLit
    }

    def ListLit: Rule1[ParsedAst.Expression.ListLit] = rule {
      SP ~ atomic("List") ~ SP ~ atomic("#{") ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~> ParsedAst.Expression.ListLit
    }

    def SetLit: Rule1[ParsedAst.Expression.SetLit] = rule {
      SP ~ atomic("Set") ~ SP ~ atomic("#{") ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~> ParsedAst.Expression.SetLit
    }

    def FMap: Rule1[ParsedAst.Expression.MapLit] = {
      def KeyValue: Rule1[(ParsedAst.Expression, ParsedAst.Expression)] = rule {
        Expression ~ optWS ~ operatorX("=>") ~ optWS ~ Expression ~> ((e1: ParsedAst.Expression, e2: ParsedAst.Expression) => (e1, e2))
      }

      rule {
        SP ~ atomic("Map") ~ SP ~ atomic("#{") ~ optWS ~ zeroOrMore(KeyValue).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~> ParsedAst.Expression.MapLit
      }
    }

    def QName: Rule1[ParsedAst.Expression.QName] = rule {
      SP ~ Names.QName ~ SP ~> ParsedAst.Expression.QName
    }

    def Open: Rule1[ParsedAst.Expression.Open] = rule {
      SP ~ keyword("open") ~ optWS ~ Names.QName ~ SP ~> ParsedAst.Expression.Open
    }

    def OpenAs: Rule1[ParsedAst.Expression.OpenAs] = rule {
      SP ~ keyword("open_as") ~ optWS ~ Names.QName ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.OpenAs
    }

    def HolyName: Rule1[ParsedAst.Expression.HolyName] = rule {
      Names.Variable ~ "?" ~ SP ~> ParsedAst.Expression.HolyName
    }

    def Hole: Rule1[ParsedAst.Expression.Hole] = {
      def AnonymousHole: Rule1[ParsedAst.Expression.Hole] = rule {
        SP ~ operatorX("???") ~ SP ~> ((sp1: SourcePosition, sp2: SourcePosition) => ParsedAst.Expression.Hole(sp1, None, sp2))
      }

      def NamedHole: Rule1[ParsedAst.Expression.Hole] = rule {
        SP ~ "?" ~ Names.Hole ~ SP ~> ((sp1: SourcePosition, name: Name.Ident, sp2: SourcePosition) => ParsedAst.Expression.Hole(sp1, Some(name), sp2))
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

    def ConstraintSet: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("#{") ~ optWS ~ zeroOrMore(Constraint) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.FixpointConstraintSet
    }

    def FixpointLambda: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("#(") ~ optWS ~ oneOrMore(PredicateParam).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ WS ~ keyword("->") ~ WS ~ Expression ~ SP ~> ParsedAst.Expression.FixpointLambda
    }

    def FixpointProject: Rule1[ParsedAst.Expression] = {
      def ExpressionPart: Rule1[Seq[ParsedAst.Expression]] = rule {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      def ProjectPart: Rule1[Seq[Name.Ident]] = rule {
        oneOrMore(Names.Predicate).separatedBy(optWS ~ "," ~ optWS)
      }

      rule {
        SP ~ (keyword("inject") | keyword("project")) ~ WS ~ ExpressionPart ~ WS ~ keyword("into") ~ WS ~ ProjectPart ~ SP ~> ParsedAst.Expression.FixpointInjectInto
      }
    }

    def FixpointSolveWithProject: Rule1[ParsedAst.Expression] = {
      def ExpressionPart: Rule1[Seq[ParsedAst.Expression]] = rule {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      def ProjectPart: Rule1[Option[Seq[Name.Ident]]] = rule {
        optional(WS ~ keyword("project") ~ WS ~ oneOrMore(Names.Predicate).separatedBy(optWS ~ "," ~ optWS))
      }

      rule {
        SP ~ keyword("solve") ~ WS ~ ExpressionPart ~ ProjectPart ~ SP ~> ParsedAst.Expression.FixpointSolveWithProject
      }
    }

    def FixpointQueryWithSelect: Rule1[ParsedAst.Expression] = {
      def ExpressionPart: Rule1[Seq[ParsedAst.Expression]] = rule {
        oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
      }

      def SelectPart: Rule1[Seq[ParsedAst.Expression]] = {
        def SingleSelect: Rule1[Seq[ParsedAst.Expression]] = rule {
          Expression ~> ((e: ParsedAst.Expression) => Seq(e))
        }

        def MultiSelect: Rule1[Seq[ParsedAst.Expression]] = rule {
          "(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
        }

        rule {
          WS ~ keyword("select") ~ WS ~ (MultiSelect | SingleSelect)
        }
      }

      def FromPart: Rule1[Seq[ParsedAst.Predicate.Body.Atom]] = rule {
        WS ~ keyword("from") ~ WS ~ oneOrMore(Predicates.Body.Atom).separatedBy(optWS ~ "," ~ optWS)
      }

      def WherePart: Rule1[Option[ParsedAst.Expression]] = rule {
        optional(WS ~ keyword("where") ~ WS ~ Expression)
      }

      rule {
        SP ~ keyword("query") ~ WS ~ ExpressionPart ~ SelectPart ~ FromPart ~ WherePart ~ SP ~> ParsedAst.Expression.FixpointQueryWithSelect
      }
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  // NB: List must be parsed before everything.
  // NB: Literal must be parsed before Variable.
  // NB: Tag must be before Literal and Variable.
  def Pattern: Rule1[ParsedAst.Pattern] = rule {
    Patterns.FCons
  }

  object Patterns {

    def Simple: Rule1[ParsedAst.Pattern] = rule {
      Lit | Var | Tag | Tuple | Record
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

    def FCons: Rule1[ParsedAst.Pattern] = rule {
      Simple ~ optional(optWS ~ SP ~ operatorX("::") ~ SP ~ optWS ~ Pattern ~> ParsedAst.Pattern.FCons)
    }

    def Record: Rule1[ParsedAst.Pattern] = {
      def RecordFieldPattern: Rule1[ParsedAst.Pattern.RecordFieldPattern] = rule {
        SP ~ Names.Field ~ optWS ~ optional("=" ~ optWS ~ Pattern) ~ SP ~> ParsedAst.Pattern.RecordFieldPattern
      }

      rule {
        SP ~ "{" ~ optWS ~ zeroOrMore(RecordFieldPattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ optional("|" ~ optWS ~ Pattern) ~ optWS ~ "}" ~ SP ~> ParsedAst.Pattern.Record
      }
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
    Predicates.Body.Atom | Predicates.Body.Guard | Predicates.Body.Functional
  }

  object Predicates {

    object Head {

      def Atom: Rule1[ParsedAst.Predicate.Head.Atom] = rule {
        SP ~ Names.Predicate ~ optWS ~ Predicates.TermList ~ SP ~> ParsedAst.Predicate.Head.Atom
      }

    }

    object Body {

      def Atom: Rule1[ParsedAst.Predicate.Body.Atom] = {
        def Polarity: Rule1[Ast.Polarity] = rule {
          (keyword("not") ~ WS ~ push(Ast.Polarity.Negative)) | push(Ast.Polarity.Positive)
        }

        def Fixity: Rule1[Ast.Fixity] = rule {
          (keyword("fix") ~ WS ~ push(Ast.Fixity.Fixed)) | push(Ast.Fixity.Loose)
        }

        rule {
          SP ~ Polarity ~ Fixity ~ Names.Predicate ~ optWS ~ Predicates.PatternList ~ SP ~> ParsedAst.Predicate.Body.Atom
        }
      }

      def Functional: Rule1[ParsedAst.Predicate.Body.Functional] = {
        def Single: Rule1[Seq[Name.Ident]] = rule {
          Names.Variable ~> ((ident: Name.Ident) => Seq(ident))
        }

        def Multi: Rule1[Seq[Name.Ident]] = rule {
          "(" ~ oneOrMore(Names.Variable).separatedBy(optWS ~ "," ~ optWS) ~ ")"
        }

        rule {
          SP ~ keyword("let") ~ WS ~ (Multi | Single) ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Predicate.Body.Functional
        }
      }

      def Guard: Rule1[ParsedAst.Predicate.Body.Guard] = rule {
        SP ~ keyword("if") ~ WS ~ Expression ~ SP ~> ParsedAst.Predicate.Body.Guard
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
      CaseUnionOrDifference ~ optional(optWS ~ atomic("->") ~ optWS ~ TypeAndEffect ~ SP ~> ParsedAst.Type.UnaryPolymorphicArrow)
    }

    def CaseUnionOrDifference: Rule1[ParsedAst.Type] = {
      def CaseUnionTail = rule {
        WS ~ atomic("++") ~ WS ~ Type ~ SP ~> ParsedAst.Type.CaseUnion
      }

      def CaseDifferenceTail = rule {
        WS ~ atomic("--") ~ WS ~ Type ~ SP ~> ParsedAst.Type.CaseDifference
      }

      rule {
        CaseIntersection ~ zeroOrMore(CaseUnionTail | CaseDifferenceTail)
      }
    }

    def CaseIntersection: Rule1[ParsedAst.Type] = rule {
      UnionOrDifference ~ zeroOrMore(WS ~ atomic("&&") ~ WS ~ Type ~ SP ~> ParsedAst.Type.CaseIntersection)
    }

    def UnionOrDifference: Rule1[ParsedAst.Type] = {
      def UnionTail = rule {
        WS ~ atomic("+") ~ WS ~ Type ~ SP ~> ParsedAst.Type.Union
      }

      def DifferenceTail = rule {
        WS ~ atomic("-") ~ WS ~ Type ~ SP ~> ParsedAst.Type.Difference
      }

      rule {
        Intersection ~ zeroOrMore(UnionTail | DifferenceTail)
      }
    }

    def Intersection: Rule1[ParsedAst.Type] = rule {
      Xor ~ zeroOrMore(WS ~ atomic("&") ~ WS ~ Type ~ SP ~> ParsedAst.Type.Intersection)
    }

    def Xor: Rule1[ParsedAst.Type] = rule {
      Or ~ zeroOrMore(WS ~ atomic("xor") ~ WS ~ Type ~ SP ~> ParsedAst.Type.Xor)
    }

    def Or: Rule1[ParsedAst.Type] = rule {
      And ~ zeroOrMore(WS ~ atomic("or") ~ WS ~ Type ~ SP ~> ParsedAst.Type.Or)
    }

    def And: Rule1[ParsedAst.Type] = rule {
      Ascribe ~ zeroOrMore(WS ~ atomic("and") ~ WS ~ Type ~ SP ~> ParsedAst.Type.And)
    }

    def Ascribe: Rule1[ParsedAst.Type] = rule {
      Apply ~ optional(WS ~ keyword(":") ~ WS ~ Kind ~ SP ~> ParsedAst.Type.Ascribe)
    }

    def Apply: Rule1[ParsedAst.Type] = rule {
      Primary ~ zeroOrMore(TypeArguments ~ SP ~> ParsedAst.Type.Apply)
    }

    def Primary: Rule1[ParsedAst.Type] = rule {
      // NB: Record must come before EffectSet as they overlap
      // NB: CaseComplement must come before Complement as they overlap
      Arrow | Tuple | Record | RecordRow | Schema | SchemaRow | CaseSet | EffectSet | Not | CaseComplement | Complement |
        Native | True | False | Pure | Impure | Var | Ambiguous
    }

    def Arrow: Rule1[ParsedAst.Type] = {
      def TypeList: Rule1[Seq[ParsedAst.Type]] = rule {
        "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
      }

      rule {
        SP ~ TypeList ~ optWS ~ (atomic("->") ~ optWS ~ TypeAndEffect ~ SP ~> ParsedAst.Type.PolymorphicArrow)
      }
    }

    def Tuple: Rule1[ParsedAst.Type] = {
      def Singleton: Rule1[ParsedAst.Type] = rule {
        "(" ~ optWS ~ Type ~ optWS ~ ")"
      }

      def Tuple: Rule1[ParsedAst.Type] = rule {
        SP ~ "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Type.Tuple
      }

      rule {
        Singleton | Tuple
      }
    }

    def Record: Rule1[ParsedAst.Type] = rule {
      SP ~ "{" ~ optWS ~ zeroOrMore(RecordFieldType).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ "}" ~ SP ~> ParsedAst.Type.Record
    }

    def RecordRow: Rule1[ParsedAst.Type] = rule {
      SP ~ "(" ~ optWS ~ zeroOrMore(RecordFieldType).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ ")" ~ SP ~> ParsedAst.Type.RecordRow
    }

    private def RecordFieldType: Rule1[ParsedAst.RecordFieldType] = rule {
      SP ~ Names.Field ~ optWS ~ "=" ~ optWS ~ Type ~ SP ~> ParsedAst.RecordFieldType
    }

    def Schema: Rule1[ParsedAst.Type] = rule {
      SP ~ atomic("#{") ~ optWS ~ zeroOrMore(RelPredicateWithTypes | LatPredicateWithTypes | PredicateWithAlias).separatedBy(optWS ~ "," ~ optWS) ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ "}" ~ SP ~> ParsedAst.Type.Schema
    }

    def SchemaRow: Rule1[ParsedAst.Type] = rule {
      SP ~ atomic("#(") ~ optWS ~ zeroOrMore(RelPredicateWithTypes | LatPredicateWithTypes | PredicateWithAlias).separatedBy(optWS ~ "," ~ optWS) ~ optional(optWS ~ "|" ~ optWS ~ Names.Variable) ~ optWS ~ ")" ~ SP ~> ParsedAst.Type.SchemaRow
    }

    def EffectSet: Rule1[ParsedAst.Type] = rule {
      SP ~ "{" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Type.EffectSet
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

    def True: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("true") ~ SP ~> ParsedAst.Type.True
    }

    def False: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("false") ~ SP ~> ParsedAst.Type.False
    }

    def Pure: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("Pure") ~ SP ~> ParsedAst.Type.Pure
    }

    def Impure: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("Impure") ~ SP ~> ParsedAst.Type.Impure
    }

    def Not: Rule1[ParsedAst.Type] = rule {
      SP ~ keyword("not") ~ WS ~ Apply ~ SP ~> ParsedAst.Type.Not
    }

    def CaseComplement: Rule1[ParsedAst.Type] = rule {
      // NB: We must not use Type here because it gives the wrong precedence.
      SP ~ "~~" ~ optWS ~ Apply ~ SP ~> ParsedAst.Type.CaseComplement
    }

    def Complement: Rule1[ParsedAst.Type] = rule {
      // NB: We must not use Type here because it gives the wrong precedence.
      SP ~ "~" ~ optWS ~ Apply ~ SP ~> ParsedAst.Type.Complement
    }

    def Var: Rule1[ParsedAst.Type] = rule {
      SP ~ Names.Variable ~ SP ~> ParsedAst.Type.Var
    }

    def Ambiguous: Rule1[ParsedAst.Type] = rule {
      SP ~ Names.QualifiedType ~ SP ~> ParsedAst.Type.Ambiguous
    }

    def CaseSet: Rule1[ParsedAst.Type] = rule {
      SP ~ "<" ~ optWS ~ zeroOrMore(Names.QName).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ">" ~ SP ~> ParsedAst.Type.CaseSet
    }

    private def TypeArguments: Rule1[Seq[ParsedAst.Type]] = rule {
      "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]"
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Kinds                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def Kind: Rule1[ParsedAst.Kind] = rule {
    Kinds.Arrow | Kinds.SimpleKind
  }

  object Kinds {

    def SimpleKind: Rule1[ParsedAst.Kind] = rule {
      Kinds.QName | Kinds.Parens
    }

    def Arrow: Rule1[ParsedAst.Kind] = rule {
      SimpleKind ~ optional(optWS ~ atomic("->") ~ optWS ~ Kind ~ SP ~> ParsedAst.Kind.Arrow)
    }

    def QName: Rule1[ParsedAst.Kind.QName] = rule {
      SP ~ Names.QName ~ SP ~> ParsedAst.Kind.QName
    }

    def Parens: Rule1[ParsedAst.Kind] = rule {
      "(" ~ Kind ~ ")"
    }

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

  def PredicateParam: Rule1[ParsedAst.PredicateParam] = rule {
    RelPredicateParam | LatPredicateParam | UntypedPredicateParam
  }

  def UntypedPredicateParam: Rule1[ParsedAst.PredicateParam.UntypedPredicateParam] = rule {
    SP ~ Names.Predicate ~ SP ~> ParsedAst.PredicateParam.UntypedPredicateParam
  }

  def RelPredicateParam: Rule1[ParsedAst.PredicateParam.RelPredicateParam] = rule {
    SP ~ Names.Predicate ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.PredicateParam.RelPredicateParam
  }

  def LatPredicateParam: Rule1[ParsedAst.PredicateParam.LatPredicateParam] = rule {
    SP ~ Names.Predicate ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ";" ~ optWS ~ Type ~ optWS ~ ")" ~ SP ~> ParsedAst.PredicateParam.LatPredicateParam
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

  def OptArgumentList: Rule1[Option[Seq[ParsedAst.Argument]]] = rule {
    optional(ArgumentList)
  }

  def AttributeList: Rule1[Seq[ParsedAst.Attribute]] = rule {
    "(" ~ optWS ~ zeroOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")"
  }

  def Annotations: Rule1[Seq[ParsedAst.Annotation]] = rule {
    zeroOrMore(Annotation).separatedBy(WS) ~ optWS
  }

  def Modifiers: Rule1[Seq[ParsedAst.Modifier]] = {
    def Inline: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("inline")) ~ SP ~> ParsedAst.Modifier
    }

    def Lawful: Rule1[ParsedAst.Modifier] = rule {
      SP ~ capture(keyword("lawful")) ~ SP ~> ParsedAst.Modifier
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

    def Modifier: Rule1[ParsedAst.Modifier] = rule {
      Inline | Lawful | Override | Public | Sealed
    }

    rule {
      zeroOrMore(Modifier).separatedBy(WS) ~ optWS
    }
  }

  def Annotation: Rule1[ParsedAst.Annotation] = rule {
    SP ~ "@" ~ Names.Annotation ~ SP ~> ParsedAst.Annotation
  }

  /////////////////////////////////////////////////////////////////////////////
  // Names                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  object Names {

    import Parser.Letters

    /**
      * A lowercase identifier is a lowercase letter optionally followed by any letter, underscore, or prime.
      */
    def LowerCaseName: Rule1[Name.Ident] = rule {
      SP ~ capture(optional("_") ~ Letters.LowerLetter ~ zeroOrMore(Letters.LegalLetter)) ~ SP ~> Name.Ident
    }

    /**
      * An uppercase identifier is an uppercase letter optionally followed by any letter, underscore, or prime.
      */
    def UpperCaseName: Rule1[Name.Ident] = rule {
      SP ~ capture(optional("_") ~ Letters.UpperLetter ~ zeroOrMore(Letters.LegalLetter)) ~ SP ~> Name.Ident
    }

    /**
      * A qualified name.
      */
    def QName: Rule1[Name.QName] = rule {
      Names.DotSeparated ~> Name.QName.fromNName _
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
      SP ~ capture(optional("_") ~ oneOrMore(Letters.GreekLetter)) ~ SP ~> Name.Ident
    }

    /**
      * A math identifier.
      */
    def MathName: Rule1[Name.Ident] = rule {
      SP ~ capture(optional("_") ~ oneOrMore(Letters.MathLetter)) ~ SP ~> Name.Ident
    }

    /**
      * An operator identifier.
      */
    def OperatorName: Rule1[Name.Ident] = rule {
      SP ~ capture(optional("_") ~ oneOrMore(Letters.OperatorLetter)) ~ SP ~> Name.Ident
    }

    /**
      * Dot-separated name.
      */
    def DotSeparated: Rule1[Name.NName] = rule {
      SP ~ oneOrMore(UpperCaseName | LowerCaseName).separatedBy(".") ~ SP ~>
        ((sp1: SourcePosition, parts: Seq[Name.Ident], sp2: SourcePosition) => Name.NName(sp1, parts.toList, sp2))
    }

    def Annotation: Rule1[Name.Ident] = rule {
      LowerCaseName | UpperCaseName
    }

    def Attribute: Rule1[Name.Ident] = LowerCaseName

    def Class: Rule1[Name.Ident] = UpperCaseName

    def QualifiedClass: Rule1[Name.QName] = QName

    def Definition: Rule1[Name.Ident] = rule {
      LowerCaseName | GreekName | MathName | OperatorName
    }

    def QualifiedDefinition: Rule1[Name.QName] = QName

    def Effect: Rule1[Name.Ident] = UpperCaseName

    def QualifiedEffect: Rule1[Name.QName] = QName

    def Operation: Rule1[Name.Ident] = LowerCaseName

    def QualifiedOperation: Rule1[Name.QName] = QName

    def Field: Rule1[Name.Ident] = LowerCaseName

    def Hole: Rule1[Name.Ident] = LowerCaseName

    def Intrinsic: Rule1[Name.Ident] = UpperCaseName

    def Predicate: Rule1[Name.Ident] = UpperCaseName

    def QualifiedPredicate: Rule1[Name.QName] = QName

    def Tag: Rule1[Name.Ident] = UpperCaseName

    def QualifiedTag: Rule1[Name.QName] = QName

    def Type: Rule1[Name.Ident] = UpperCaseName

    def QualifiedType: Rule1[Name.QName] = QName

    def Variable: Rule1[Name.Ident] = rule {
      LowerCaseName | GreekName | MathName | Wildcard
    }

    def JavaIdentifier: Rule1[String] = rule {
      capture((CharPredicate.Alpha | anyOf("_$")) ~ zeroOrMore(CharPredicate.AlphaNum | anyOf("_$")))
    }

    def JavaName: Rule1[Name.JavaName] = rule {
      SP ~ oneOrMore(JavaIdentifier).separatedBy(".") ~ SP ~> Name.JavaName
    }

    def JavaMethod: Rule1[Name.Ident] = rule {
      SP ~ JavaIdentifier ~ SP ~> Name.Ident
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Operator                                                                //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Reads the symbol and captures its source location.
    */
  def operator(symbol: String): Rule1[ParsedAst.Operator] = namedRule(symbol) {
    SP ~ capture(atomic(symbol)) ~ SP ~> ParsedAst.Operator
  }


  /**
    * Reads the symbol but does not capture it.
    * Used for operators that are not converted to binary expressions.
    *
    * This function is exactly the same as using [[atomic]].
    * However, it is useful for identifying symbols that should be reserved.
    */
  private def operatorX(symbol: String): Rule0 = namedRule(symbol) {
    atomic(symbol)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Keyword                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Reads the keyword and looks ahead to ensure there no legal letters immediately following.
    */
  def keyword(word: String): Rule0 = namedRule(s"${'"'}$word${'"'}") {
    atomic(word) ~ !Parser.Letters.LegalLetter
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
    quiet(oneOrMore(" " | "\t" | NewLine | Comment))
  }

  def PossibleDocComment: Rule1[Option[ParsedAst.Doc]] = rule {
    Comments.DocCommentBlock ~> {
      Some(_)
    } | Comment ~ push(None)
  }

  def optWS: Rule0 = rule {
    quiet(optional(WS))
  }

  def PureWS: Rule0 = rule {
    zeroOrMore(" " | "\t" | NewLine)
  }

  def ToplevelOptWS: Rule1[Seq[ParsedAst.Doc]] = rule {
    PureWS ~ zeroOrMore(PossibleDocComment).separatedBy(PureWS) ~ PureWS ~> {
      (s: Seq[Option[ParsedAst.Doc]]) => s.flatten
    }
  }

  def NewLine: Rule0 = rule {
    quiet("\n" | "\r\n" | "\r")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Documentation                                                           //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Optionally a parses a documentation comment.
    */
  def Documentation: Rule1[ParsedAst.Doc] = rule {
    SP ~ ToplevelOptWS ~ SP ~> {
      (sp1: SourcePosition, seq: Seq[ParsedAst.Doc], sp2: SourcePosition) =>
        seq.lastOption.getOrElse(ParsedAst.Doc(sp1, Seq.empty, sp2))
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

    def DocCommentLine: Rule1[String] = rule {
      atomic("///") ~ capture(zeroOrMore(!NewLine ~ ANY)) ~ (NewLine | EOI)
    }

    def DocCommentBlock: Rule1[ParsedAst.Doc] = rule {
      SP ~ oneOrMore(DocCommentLine).separatedBy(zeroOrMore(" " | "\t")) ~ SP ~> ParsedAst.Doc
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
