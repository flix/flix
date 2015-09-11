package ca.uwaterloo.flix.lang

import java.nio.file.Path

import ca.uwaterloo.flix.lang.ast._
import ca.uwaterloo.flix.util.misc.Unoptimized

import org.parboiled2._

import scala.collection.immutable.Seq

// TODO: Ensure that seperatedBy allows for optWS "," optWS

/**
 * A PEG parser for the Flix programming language.
 */
class Parser(val path: Option[Path], val input: ParserInput) extends org.parboiled2.Parser {

  // TODO: Use atomic keyword
  // TODO: Tags

  def Root: Rule1[ParsedAst.Root] = rule {
    optWS ~ zeroOrMore(Declaration) ~ optWS ~ EOI ~> ParsedAst.Root
  }

  // NB: RuleDeclaration must be parsed before FactDeclaration.
  def Declaration: Rule1[ParsedAst.Declaration] = rule {
    NamespaceDeclaration | TypeDeclaration | VariableDeclaration | ValueDeclaration | FunctionDeclaration | EnumDeclaration | RuleDeclaration | FactDeclaration
  }

  def NamespaceDeclaration: Rule1[ParsedAst.Declaration.Namespace] = rule {
    atomic("namespace") ~ WS ~ QName ~ optWS ~ '{' ~ optWS ~ zeroOrMore(Declaration) ~ optWS ~ '}' ~ optSC ~> ParsedAst.Declaration.Namespace
  }

  def TypeDeclaration: Rule1[ParsedAst.Declaration.Tpe] = rule {
    "type" ~ WS ~ Ident ~ optWS ~ "=" ~ optWS ~ Type ~ ";" ~ optWS ~> ParsedAst.Declaration.Tpe
  }

  def ValueDeclaration: Rule1[ParsedAst.Declaration.Val] = rule {
    "val" ~ WS ~ Ident ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ ";" ~ optWS ~> ParsedAst.Declaration.Val
  }

  // TODO
  def VariableDeclaration: Rule1[ParsedAst.Declaration.Var] = rule {
    "var" ~ WS ~ Ident ~ ":" ~ optWS ~ Type ~ ";" ~ optWS ~> ParsedAst.Declaration.Var
  }

  def FunctionDeclaration: Rule1[ParsedAst.Declaration.Fun] = rule {
    zeroOrMore(Annotation) ~ "def" ~ WS ~ Ident ~ "(" ~ ArgumentList ~ ")" ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ ";" ~ optWS ~> ParsedAst.Declaration.Fun
  }

  def EnumDeclaration: Rule1[ParsedAst.Declaration.Enum] = rule {
    "enum" ~ WS ~ Ident ~ optWS ~ "{" ~ optWS ~ EnumBody ~ optWS ~ "}" ~ optWS ~ ";" ~ optWS ~> ParsedAst.Declaration.Enum
  }

  def EnumBody: Rule1[Seq[ParsedAst.Type.Tag]] = rule {
    oneOrMore("case" ~ WS ~ Ident ~> ParsedAst.Type.Tag).separatedBy("," ~ optWS)
  }

  // TODO: Use separate thing for tags.

  //def LatticeDeclaration: Rule1[Ast.Declaration.Lattice] = rule {
  //   "lat" ~ WS ~ Ident ~ optWS ~ "=" ~ optWS ~ RecordExp ~ ";" ~ optWS ~> Ast.Declaration.Lattice
  // }


  /** *************************************************************************/
  /** Expressions                                                           ***/
  /** *************************************************************************/
  def Expression: Rule1[ParsedAst.Expression] = rule {
    LogicalExpression
  }

  def LogicalExpression: Rule1[ParsedAst.Expression] = rule {
    ComparisonExpression ~ optional(optWS ~ LogicalOp ~ optWS ~ ComparisonExpression ~> ParsedAst.Expression.Binary)
  }

  def ComparisonExpression: Rule1[ParsedAst.Expression] = rule {
    AdditiveExpression ~ optional(optWS ~ ComparisonOp ~ optWS ~ AdditiveExpression ~> ParsedAst.Expression.Binary)
  }

  def AdditiveExpression: Rule1[ParsedAst.Expression] = rule {
    MultiplicativeExpression ~ zeroOrMore(optWS ~ AdditiveOp ~ optWS ~ MultiplicativeExpression ~> ParsedAst.Expression.Binary)
  }

  def MultiplicativeExpression: Rule1[ParsedAst.Expression] = rule {
    InfixExpression ~ zeroOrMore(optWS ~ MultiplicativeOp ~ optWS ~ InfixExpression ~> ParsedAst.Expression.Binary)
  }

  def InfixExpression: Rule1[ParsedAst.Expression] = rule {
    UnaryExpression ~ optional(optWS ~ "`" ~ QName ~ "`" ~ optWS ~ UnaryExpression ~> ParsedAst.Expression.Infix)
  }

  def UnaryExpression: Rule1[ParsedAst.Expression] = rule {
    (UnaryOp ~ optWS ~ UnaryExpression ~> ParsedAst.Expression.Unary) | AscribeExpression
  }

  def AscribeExpression: Rule1[ParsedAst.Expression] = rule {
    SimpleExpression ~ optWS ~ ":" ~ optWS ~ Type ~> ParsedAst.Expression.Ascribe | SimpleExpression
  }

  def SimpleExpression: Rule1[ParsedAst.Expression] = rule {
    LiteralExpression | LetExpression | IfThenElseExpression | MatchExpression | TupleExpression | LambdaExpression | ApplyExpression | VariableExpression | ErrorExpression
  }

  def LiteralExpression: Rule1[ParsedAst.Expression.Lit] = rule {
    Literal ~> ParsedAst.Expression.Lit
  }

  def LetExpression: Rule1[ParsedAst.Expression.Let] = rule {
    atomic("let") ~ WS ~ Ident ~ optWS ~ "=" ~ optWS ~ Expression ~ WS ~ atomic("in") ~ WS ~ Expression ~> ParsedAst.Expression.Let
  }

  def IfThenElseExpression: Rule1[ParsedAst.Expression.IfThenElse] = rule {
    atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ WS ~ Expression ~ WS ~ atomic("else") ~ WS ~ Expression ~> ParsedAst.Expression.IfThenElse
  }

  def MatchExpression: Rule1[ParsedAst.Expression.Match] = {
    def MatchRule: Rule1[(ParsedAst.Pattern, ParsedAst.Expression)] = rule {
      atomic("case") ~ WS ~ Pattern ~ WS ~ atomic("=>") ~ WS ~ Expression ~ optSC ~> ((p: ParsedAst.Pattern, e: ParsedAst.Expression) => (p, e))
    }

    rule {
      atomic("match") ~ WS ~ Expression ~ WS ~ atomic("with") ~ optWS ~ "{" ~ WS ~ oneOrMore(MatchRule) ~ "}" ~> ParsedAst.Expression.Match
    }
  }

  def ApplyExpression: Rule1[ParsedAst.Expression.AmbiguousApply] = rule {
    QName ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~> ParsedAst.Expression.AmbiguousApply
  }

  def TupleExpression: Rule1[ParsedAst.Expression] = {
    def Unit: Rule1[ParsedAst.Expression] = rule {
      atomic("()") ~> (() => ParsedAst.Expression.Lit(ParsedAst.Literal.Unit))
    }

    def Singleton: Rule1[ParsedAst.Expression] = rule {
      "(" ~ optWS ~ Expression ~ optWS ~ ")"
    }

    def Tuple: Rule1[ParsedAst.Expression] = rule {
      "(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~> ParsedAst.Expression.Tuple
    }

    rule {
      Unit | Singleton | Tuple
    }
  }

  def VariableExpression: Rule1[ParsedAst.Expression.AmbiguousVar] = rule {
    QName ~> ParsedAst.Expression.AmbiguousVar
  }

  def LambdaExpression: Rule1[ParsedAst.Expression.Lambda] = rule {
    atomic("fn") ~ optWS ~ "(" ~ ArgumentList ~ ")" ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~> ParsedAst.Expression.Lambda
  }

  def ErrorExpression: Rule1[ParsedAst.Expression] = rule {
    SourceLocation ~ atomic("???") ~> ParsedAst.Expression.Error
  }

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  // NB: LiteralPattern must be parsed before VariablePattern.
  def Pattern: Rule1[ParsedAst.Pattern] = rule {
    WildcardPattern | LiteralPattern | VariablePattern | TuplePattern
  }

  def WildcardPattern: Rule1[ParsedAst.Pattern.Wildcard] = rule {
    SourceLocation ~ atomic("_") ~> ParsedAst.Pattern.Wildcard
  }

  def VariablePattern: Rule1[ParsedAst.Pattern.Var] = rule {
    Ident ~> ParsedAst.Pattern.Var
  }

  def LiteralPattern: Rule1[ParsedAst.Pattern.Lit] = rule {
    Literal ~> ParsedAst.Pattern.Lit
  }

  def TuplePattern: Rule1[ParsedAst.Pattern.Tuple] = rule {
    "(" ~ oneOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~> ParsedAst.Pattern.Tuple
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts and Rules                                                         //
  /////////////////////////////////////////////////////////////////////////////
  def FactDeclaration: Rule1[ParsedAst.Declaration.Fact] = rule {
    AmbiguousPredicate ~ optWS ~ "." ~ optWS ~> ParsedAst.Declaration.Fact
  }

  def RuleDeclaration: Rule1[ParsedAst.Declaration.Rule] = rule {
    AmbiguousPredicate ~ optWS ~ ":-" ~ optWS ~ oneOrMore(AmbiguousPredicate).separatedBy(optWS ~ "," ~ optWS) ~ "." ~ optWS ~> ParsedAst.Declaration.Rule
  }

  def AmbiguousPredicate: Rule1[ParsedAst.AmbiguousPredicate] = rule {
    QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ optWS ~> ParsedAst.AmbiguousPredicate
  }

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // NB: ApplyTerm must be parsed before VariableTerm.
  def Term: Rule1[ParsedAst.Term] = rule {
    ApplyTerm | WildcardTerm | VariableTerm | LiteralTerm
  }

  def WildcardTerm: Rule1[ParsedAst.Term] = rule {
    SourceLocation ~ atomic("_") ~> ParsedAst.Term.Wildcard
  }

  def VariableTerm: Rule1[ParsedAst.Term.Var] = rule {
    Ident ~> ParsedAst.Term.Var
  }

  def LiteralTerm: Rule1[ParsedAst.Term.Lit] = rule {
    Literal ~> ParsedAst.Term.Lit
  }

  def ApplyTerm: Rule1[ParsedAst.Term.Apply] = rule {
    QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy("," ~ optWS) ~ ")" ~> ParsedAst.Term.Apply
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // NB: The parser works left-to-right, but the inline code ensures that the
  // function types are right-associative.
  def Type: Rule1[ParsedAst.Type] = rule {
    oneOrMore(SimpleType).separatedBy(optWS ~ "->" ~ optWS) ~> ((types: Seq[ParsedAst.Type]) => types match {
      case xs if xs.size == 1 => xs.head
      case xs => xs.reduceRight[ParsedAst.Type](ParsedAst.Type.Function)
    })
  }

  // NB: ParametricType must be parsed before AmbiguousType.
  def SimpleType: Rule1[ParsedAst.Type] = rule {
    ParametricType | AmbiguousType | TupleType
  }

  def AmbiguousType: Rule1[ParsedAst.Type.Ambiguous] = rule {
    QName ~> ParsedAst.Type.Ambiguous
  }

  def ParametricType: Rule1[ParsedAst.Type.Parametric] = rule {
    QName ~ optWS ~ "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ optWS ~> ParsedAst.Type.Parametric
  }

  def TupleType: Rule1[ParsedAst.Type] = {
    def Unit: Rule1[ParsedAst.Type] = rule {
      atomic("()") ~ optWS ~> (() => ParsedAst.Type.Unit)
    }

    def Singleton: Rule1[ParsedAst.Type] = rule {
      "(" ~ optWS ~ Type ~ optWS ~ ")" ~ optWS
    }

    def Tuple: Rule1[ParsedAst.Type] = rule {
      "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~> ParsedAst.Type.Tuple
    }

    rule {
      Unit | Singleton | Tuple
    }
  }

  /** *************************************************************************/
  /** Helpers                                                               ***/
  /** *************************************************************************/
  // TODO: Can we get rid of these?
  def ArgumentList: Rule1[Seq[(ParsedAst.Ident, ParsedAst.Type)]] = rule {
    zeroOrMore(Argument).separatedBy("," ~ optWS)
  }

  def Argument: Rule1[(ParsedAst.Ident, ParsedAst.Type)] = rule {
    Ident ~ ":" ~ optWS ~ Type ~> ((name: ParsedAst.Ident, typ: ParsedAst.Type) => (name, typ))
  }

  def Annotation: Rule1[ParsedAst.Ident] = rule {
    "@" ~ Ident ~ WS
  }


  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  def LegalIdentifier: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") ~ zeroOrMore("'"))
  }

  def Ident: Rule1[ParsedAst.Ident] = rule {
    SourceLocation ~ LegalIdentifier ~>
      ((location: SourceLocation, name: String) => ParsedAst.Ident(name, location))
  }

  // TODO: Probably need to introduce SName so we can handle names like foo and Foo.bar for tags.
  // TODO: But a function parameter should not be allowed to be a SName...
  // TODO: Another option is to simple parse (QName) . Foo as a special expression and pattern?

  def QName: Rule1[ParsedAst.QName] = rule {
    SourceLocation ~ oneOrMore(LegalIdentifier).separatedBy(atomic("::")) ~>
      ((location: SourceLocation, parts: Seq[String]) => ParsedAst.QName(parts, location))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    UnitLiteral | BoolLiteral | IntLiteral | StrLiteral
  }

  def UnitLiteral: Rule1[ParsedAst.Literal.Unit.type] = rule {
    atomic("()") ~> (() => ParsedAst.Literal.Unit)
  }

  def BoolLiteral: Rule1[ParsedAst.Literal.Bool] = rule {
    atomic("true") ~> (() => ParsedAst.Literal.Bool(literal = true)) | atomic("false") ~> (() => ParsedAst.Literal.Bool(literal = false))
  }

  def IntLiteral: Rule1[ParsedAst.Literal.Int] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((x: String) => ParsedAst.Literal.Int(x.toInt))
  }

  def StrLiteral: Rule1[ParsedAst.Literal.Str] = rule {
    "\"" ~ capture(zeroOrMore(!"\"" ~ CharPredicate.Printable)) ~ "\"" ~> ParsedAst.Literal.Str
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
  def SourceLocation: Rule1[SourceLocation] = {
    val position = Position(cursor, input)
    rule {
      push(ast.SourceLocation(path, position.line, position.column))
    }
  }

}
