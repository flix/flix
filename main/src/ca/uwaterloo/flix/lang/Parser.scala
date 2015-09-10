package ca.uwaterloo.flix.lang

import java.nio.file.{Files, Path}

import ca.uwaterloo.flix.lang
import ca.uwaterloo.flix.lang.ast.{Ast, BinaryOperator, UnaryOperator}
import ca.uwaterloo.flix.util.misc.Unoptimized
import org.parboiled2._

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.{Failure, Success}

object Parser {

  /**
   * Returns the abstract syntax tree of the given `paths`.
   */
  def parse(paths: Traversable[Path]): Ast.Root = {
    val asts = paths map parse
    asts.reduce[Ast.Root] {
      case (ast1, ast2) => Ast.Root(ast1.declarations ++ ast2.declarations)
    }
  }

  /**
   * Returns the abstract syntax tree of the given `path`.
   */
  def parse(path: Path): Ast.Root =
    if (!Files.exists(path))
      throw new RuntimeException(s"Path '$path' does not exist.")
    else if (!Files.isReadable(path))
      throw new RuntimeException(s"Path '$path' is not readable.")
    else if (!Files.isRegularFile(path))
      throw new RuntimeException(s"Path '$path' is not a regular file.")
    else
      parse(Source.fromFile(path.toFile).getLines().mkString("\n"))

  /**
   * Returns the abstract syntax tree of the given string `input`.
   */
  def parse(input: String): Ast.Root = {
    val parser = new Parser(None, input)
    parser.Root.run() match {
      case Success(ast) => ast
      case Failure(e: ParseError) => throw new RuntimeException(parser.formatError(e))
      case Failure(e) => throw new RuntimeException("Unexpected error during parsing run: " + e)
    }
  }
}

// TODO: Ensure that seperatedBy allows for optWS "," optWS

/**
 * A PEG parser for the Flix programming language.
 */
class Parser(val path: Option[Path], val input: ParserInput) extends org.parboiled2.Parser {

  // TODO: Use atomic keyword
  // TODO: Tags

  def Root: Rule1[Ast.Root] = rule {
    optWS ~ zeroOrMore(Declaration) ~ optWS ~ EOI ~> Ast.Root
  }

  // NB: RuleDeclaration must be parsed before FactDeclaration.
  def Declaration: Rule1[Ast.Declaration] = rule {
    NamespaceDeclaration | TypeDeclaration | VariableDeclaration | ValueDeclaration | FunctionDeclaration | EnumDeclaration | LatticeDeclaration | RuleDeclaration | FactDeclaration
  }

  def NamespaceDeclaration: Rule1[Ast.Declaration.Namespace] = rule {
    atomic("namespace") ~ WS ~ QName ~ optWS ~ '{' ~ optWS ~ zeroOrMore(Declaration) ~ optWS ~ '}' ~ optSC ~> Ast.Declaration.Namespace
  }

  def TypeDeclaration: Rule1[Ast.Declaration.Tpe] = rule {
    "type" ~ WS ~ Ident ~ optWS ~ "=" ~ optWS ~ Type ~ ";" ~ optWS ~> Ast.Declaration.Tpe
  }

  def ValueDeclaration: Rule1[Ast.Declaration.Val] = rule {
    "val" ~ WS ~ Ident ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ ";" ~ optWS ~> Ast.Declaration.Val
  }

  // TODO
  def VariableDeclaration: Rule1[Ast.Declaration.Var] = rule {
    "var" ~ WS ~ Ident ~ ":" ~ optWS ~ Type ~ ";" ~ optWS ~> Ast.Declaration.Var
  }

  def FunctionDeclaration: Rule1[Ast.Declaration.Fun] = rule {
    zeroOrMore(Annotation) ~ "def" ~ WS ~ Ident ~ "(" ~ ArgumentList ~ ")" ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ ";" ~ optWS ~> Ast.Declaration.Fun
  }

  def EnumDeclaration: Rule1[Ast.Declaration.Enum] = rule {
    "enum" ~ WS ~ Ident ~ optWS ~ "{" ~ optWS ~ EnumBody ~ optWS ~ "}" ~ optWS ~ ";" ~ optWS ~> Ast.Declaration.Enum
  }

  def EnumBody: Rule1[Seq[Ast.Type.Tag]] = rule {
    oneOrMore("case" ~ WS ~ Ident ~> Ast.Type.Tag).separatedBy("," ~ optWS)
  }

  // TODO: Use separate thing for tags.

  def LatticeDeclaration: Rule1[Ast.Declaration.Lattice] = rule {
    "lat" ~ WS ~ Ident ~ optWS ~ "=" ~ optWS ~ RecordExp ~ ";" ~ optWS ~> Ast.Declaration.Lattice
  }


  /** *************************************************************************/
  /** Expressions                                                           ***/
  /** *************************************************************************/
  // TODO: Rename Exp -> Expression
  def Expression: Rule1[Ast.Expression] = rule {
    InfixExp
  }

  def InfixExp: Rule1[Ast.Expression] = rule {
    LogicalExp ~ optional(optWS ~ "`" ~ QName ~ "`" ~ optWS ~ LogicalExp ~> Ast.Expression.Infix)
  }

  def LogicalExp: Rule1[Ast.Expression] = rule {
    ComparisonExp ~ zeroOrMore(optWS ~ LogicalOp ~ optWS ~ ComparisonExp ~> Ast.Expression.Binary)
  }

  def ComparisonExp: Rule1[Ast.Expression] = rule {
    MultiplicativeExp ~ optional(optWS ~ ComparisonOp ~ optWS ~ MultiplicativeExp ~> Ast.Expression.Binary)
  }

  def MultiplicativeExp: Rule1[Ast.Expression] = rule {
    AdditiveExp ~ zeroOrMore(optWS ~ MultiplicativeOp ~ optWS ~ AdditiveExp ~> Ast.Expression.Binary)
  }

  def AdditiveExp: Rule1[Ast.Expression] = rule {
    SimpleExpression ~ zeroOrMore(optWS ~ AdditiveOp ~ optWS ~ SimpleExpression ~> Ast.Expression.Binary)
  }

  def SimpleExpression: Rule1[Ast.Expression] = rule {
    LiteralExp | LetExp | IfThenElseExp | MatchExp | TupleExp | LambdaExp | CallExp | VariableExp | ErrorExp
  }

  def LiteralExp: Rule1[Ast.Expression.Lit] = rule {
    Literal ~> Ast.Expression.Lit
  }

  def LetExp: Rule1[Ast.Expression.Let] = rule {
    atomic("let") ~ WS ~ Ident ~ optWS ~ "=" ~ optWS ~ Expression ~ WS ~ atomic("in") ~ WS ~ Expression ~> Ast.Expression.Let
  }

  def IfThenElseExp: Rule1[Ast.Expression.IfThenElse] = rule {
    atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ WS ~ Expression ~ WS ~ atomic("else") ~ WS ~ Expression ~> Ast.Expression.IfThenElse
  }

  def MatchExp: Rule1[Ast.Expression.Match] = rule {
    atomic("match") ~ WS ~ Expression ~ WS ~ atomic("with") ~ optWS ~ "{" ~ WS ~ oneOrMore(MatchRule) ~ "}" ~> Ast.Expression.Match
  }

  def MatchRule: Rule1[(Ast.Pattern, Ast.Expression)] = rule {
    atomic("case") ~ WS ~ Pattern ~ WS ~ atomic("=>") ~ WS ~ Expression ~ optSC ~> ((p: Ast.Pattern, e: Ast.Expression) => (p, e))
  }

  def CallExp: Rule1[Ast.Expression.AmbiguousCall] = rule {
    QName ~ "(" ~ zeroOrMore(Expression).separatedBy("," ~ optWS) ~ ")" ~> Ast.Expression.AmbiguousCall
  }

  def TupleExp: Rule1[Ast.Expression.Tuple] = rule {
    "(" ~ oneOrMore(Expression).separatedBy("," ~ optWS) ~ ")" ~> Ast.Expression.Tuple
  }

  def RecordExp: Rule1[Ast.Expression.Record] = rule {
    "record" ~ WS ~ "{" ~ optWS ~ zeroOrMore(RecordKeyValue).separatedBy("," ~ optWS) ~ optWS ~ "}" ~> Ast.Expression.Record
  }

  private def RecordKeyValue: Rule1[(Ast.Ident, Ast.Expression)] = rule {
    Ident ~ optWS ~ "=" ~ optWS ~ Expression ~> ((k: Ast.Ident, v: Ast.Expression) => (k, v))
  }

  def VariableExp: Rule1[Ast.Expression.AmbiguousName] = rule {
    QName ~> Ast.Expression.AmbiguousName
  }

  def LambdaExp: Rule1[Ast.Expression.Lambda] = rule {
    "fn" ~ optWS ~ "(" ~ ArgumentList ~ "):" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~> Ast.Expression.Lambda
  }

  def ErrorExp: Rule1[Ast.Expression] = rule {
    SourceLocation ~ atomic("???") ~> Ast.Expression.Error
  }

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  // NB: LiteralPattern must be parsed before VariablePattern.
  def Pattern: Rule1[Ast.Pattern] = rule {
    WildcardPattern | LiteralPattern | VariablePattern | TuplePattern
  }

  def WildcardPattern: Rule1[Ast.Pattern.Wildcard] = rule {
    SourceLocation ~ atomic("_") ~> Ast.Pattern.Wildcard
  }

  def VariablePattern: Rule1[Ast.Pattern.Var] = rule {
    Ident ~> Ast.Pattern.Var
  }

  def LiteralPattern: Rule1[Ast.Pattern.Lit] = rule {
    Literal ~> Ast.Pattern.Lit
  }

  def TuplePattern: Rule1[Ast.Pattern.Tuple] = rule {
    "(" ~ oneOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~> Ast.Pattern.Tuple
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts and Rules                                                         //
  /////////////////////////////////////////////////////////////////////////////
  def FactDeclaration: Rule1[Ast.Declaration.Fact] = rule {
    AmbiguousPredicate ~ optWS ~ "." ~ optWS ~> Ast.Declaration.Fact
  }

  def RuleDeclaration: Rule1[Ast.Declaration.Rule] = rule {
    AmbiguousPredicate ~ optWS ~ ":-" ~ optWS ~ oneOrMore(AmbiguousPredicate).separatedBy(optWS ~ "," ~ optWS) ~ "." ~ optWS ~> Ast.Declaration.Rule
  }

  def AmbiguousPredicate: Rule1[Ast.AmbiguousPredicate] = rule {
    QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ optWS ~> Ast.AmbiguousPredicate
  }

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // NB: ApplyTerm must be parsed before VariableTerm.
  def Term: Rule1[Ast.Term] = rule {
    ApplyTerm | WildcardTerm | VariableTerm | LiteralTerm
  }

  def WildcardTerm: Rule1[Ast.Term] = rule {
    SourceLocation ~ atomic("_") ~> Ast.Term.Wildcard
  }

  def VariableTerm: Rule1[Ast.Term.Var] = rule {
    Ident ~> Ast.Term.Var
  }

  def LiteralTerm: Rule1[Ast.Term.Lit] = rule {
    Literal ~> Ast.Term.Lit
  }

  def ApplyTerm: Rule1[Ast.Term.Apply] = rule {
    QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy("," ~ optWS) ~ ")" ~> Ast.Term.Apply
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // NB: The parser works left-to-right, but the inline code ensures that the
  // function types are right-associative.
  def Type: Rule1[Ast.Type] = rule {
    oneOrMore(SimpleType).separatedBy(optWS ~ "->" ~ optWS) ~> ((types: Seq[Ast.Type]) => types match {
      case xs if xs.size == 1 => xs.head
      case xs => xs.reduceRight[Ast.Type](Ast.Type.Function)
    })
  }

  // NB: ParametricType must be parsed before AmbiguousType.
  def SimpleType: Rule1[Ast.Type] = rule {
    ParametricType | AmbiguousType | TupleType
  }

  def AmbiguousType: Rule1[Ast.Type.Ambiguous] = rule {
    QName ~> Ast.Type.Ambiguous
  }

  def ParametricType: Rule1[Ast.Type.Parametric] = rule {
    QName ~ optWS ~ "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ optWS ~> Ast.Type.Parametric
  }

  def TupleType: Rule1[Ast.Type] = {
    def Unit: Rule1[Ast.Type] = rule {
      "()" ~ optWS ~> (() => Ast.Type.Unit)
    }

    def Singleton: Rule1[Ast.Type] = rule {
      "(" ~ optWS ~ Type ~ optWS ~ ")" ~ optWS
    }

    def Tuple: Rule1[Ast.Type] = rule {
      "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~> Ast.Type.Tuple
    }

    rule {
      Unit | Singleton | Tuple
    }
  }

  /** *************************************************************************/
  /** Helpers                                                               ***/
  /** *************************************************************************/
  // TODO: Can we get rid of these?
  def ArgumentList: Rule1[Seq[(Ast.Ident, Ast.Type)]] = rule {
    zeroOrMore(Argument).separatedBy("," ~ optWS)
  }

  def Argument: Rule1[(Ast.Ident, Ast.Type)] = rule {
    Ident ~ ":" ~ optWS ~ Type ~> ((name: Ast.Ident, typ: Ast.Type) => (name, typ))
  }

  def Annotation: Rule1[Ast.Ident] = rule {
    "@" ~ Ident ~ WS
  }


  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  def LegalIdentifier: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") ~ zeroOrMore("'"))
  }

  def Ident: Rule1[Ast.Ident] = rule {
    SourceLocation ~ LegalIdentifier ~>
      ((location: lang.SourceLocation, name: String) => Ast.Ident(name, location))
  }

  // TODO: Probably need to introduce SName so we can handle names like foo and Foo.bar for tags.
  // TODO: But a function parameter should not be allowed to be a SName...
  // TODO: Another option is to simple parse (QName) . Foo as a special expression and pattern?

  def QName: Rule1[Ast.QName] = rule {
    SourceLocation ~ oneOrMore(LegalIdentifier).separatedBy(atomic("::")) ~>
      ((location: lang.SourceLocation, parts: Seq[String]) => Ast.QName(parts, location))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[Ast.Literal] = rule {
    UnitLiteral | BoolLiteral | IntLiteral | StrLiteral
  }

  def UnitLiteral: Rule1[Ast.Literal.Unit.type] = rule {
    atomic("()") ~> (() => Ast.Literal.Unit)
  }

  def BoolLiteral: Rule1[Ast.Literal.Bool] = rule {
    atomic("true") ~> (() => Ast.Literal.Bool(literal = true)) | atomic("false") ~> (() => Ast.Literal.Bool(literal = false))
  }

  def IntLiteral: Rule1[Ast.Literal.Int] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((x: String) => Ast.Literal.Int(x.toInt))
  }

  def StrLiteral: Rule1[Ast.Literal.Str] = rule {
    "\"" ~ capture(zeroOrMore(!"\"" ~ CharPredicate.Printable)) ~ "\"" ~> Ast.Literal.Str
  }

  /////////////////////////////////////////////////////////////////////////////
  // Operators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  def UnaryOp: Rule1[UnaryOperator] = rule {
    str("!") ~> (() => UnaryOperator.Not) |
      str("+") ~> (() => UnaryOperator.UnaryPlus) |
      str("-") ~> (() => UnaryOperator.UnaryMinus)
  }

  def LogicalOp: Rule1[BinaryOperator] = rule {
    str("&&") ~> (() => BinaryOperator.And) |
      str("||") ~> (() => BinaryOperator.Or)
  }

  def ComparisonOp: Rule1[BinaryOperator] = rule {
    str("<=") ~> (() => BinaryOperator.LessEqual) |
      str(">=") ~> (() => BinaryOperator.GreaterEqual) |
      str("<") ~> (() => BinaryOperator.Less) |
      str(">") ~> (() => BinaryOperator.Greater) |
      str("==") ~> (() => BinaryOperator.Equal) |
      str("!=") ~> (() => BinaryOperator.NotEqual)
  }

  def MultiplicativeOp: Rule1[BinaryOperator] = rule {
    str("*") ~> (() => BinaryOperator.Times) |
      str("/") ~> (() => BinaryOperator.Divide) |
      str("%") ~> (() => BinaryOperator.Modulo)
  }

  def AdditiveOp: Rule1[BinaryOperator] = rule {
    str("+") ~> (() => BinaryOperator.Plus) |
      str("-") ~> (() => BinaryOperator.Minus)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Whitespace                                                              //
  /////////////////////////////////////////////////////////////////////////////
  def WS: Rule0 = rule {
    oneOrMore(" " | "\t" | NewLine | SingleLineComment | MultiLineComment)
  }

  def optWS: Rule0 = rule {
    optional(WS)
  }

  def optSC: Rule0 = rule {
    optWS ~ optional(";") ~ optWS
  }

  def NewLine: Rule0 = rule {
    "\n" | "\r\n"
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
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

  /////////////////////////////////////////////////////////////////////////////
  // Source Location                                                         //
  /////////////////////////////////////////////////////////////////////////////
  @Unoptimized
  def SourceLocation: Rule1[lang.SourceLocation] = {
    val position = Position(cursor, input)
    rule {
      push(lang.SourceLocation(path, position.line, position.column))
    }
  }

}
