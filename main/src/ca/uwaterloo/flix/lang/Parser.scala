package ca.uwaterloo.flix.lang

import java.nio.file.Path

import ca.uwaterloo.flix.lang.ast._
import ca.uwaterloo.flix.util.misc.Unoptimized

import org.parboiled2._

import scala.collection.immutable.Seq

// TODO: Dealing with whitespace is hard. Figure out a good way.

// TODO: Ensure that seperatedBy allows for optWS "," optWS

/**
 * A parser for the Flix language.
 */
class Parser(val path: Option[Path], val input: ParserInput) extends org.parboiled2.Parser {

  // TODO: Use atomic keyword
  // TODO: Tags

  def Root: Rule1[ParsedAst.Root] = rule {
    optWS ~ zeroOrMore(Declaration) ~ optWS ~ EOI ~> ParsedAst.Root
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations                                                            //
  /////////////////////////////////////////////////////////////////////////////
  // NB: RuleDeclaration must be parsed before FactDeclaration.
  def Declaration: Rule1[ParsedAst.Declaration] = rule {
    NamespaceDeclaration | TypeDeclaration | ValueDeclaration | FunctionDeclaration | EnumDeclaration |
      LatticeDeclaration | RelationDeclaration | RuleDeclaration | FactDeclaration
  }

  def NamespaceDeclaration: Rule1[ParsedAst.Declaration.Namespace] = rule {
    atomic("namespace") ~ WS ~ QName ~ optWS ~ '{' ~ optWS ~ zeroOrMore(Declaration) ~ optWS ~ '}' ~ optSC ~> ParsedAst.Declaration.Namespace
  }

  def TypeDeclaration: Rule1[ParsedAst.Declaration.Tpe] = rule {
    atomic("type") ~ WS ~ Ident ~ optWS ~ "=" ~ optWS ~ Type ~ optSC ~> ParsedAst.Declaration.Tpe
  }

  def ValueDeclaration: Rule1[ParsedAst.Declaration.Val] = rule {
    atomic("val") ~ WS ~ Ident ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ optSC ~> ParsedAst.Declaration.Val
  }

  def FunctionDeclaration: Rule1[ParsedAst.Declaration.Fun] = rule {
    atomic("def") ~ WS ~ Ident ~ optWS ~ "(" ~ ArgumentList ~ ")" ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ optSC ~> ParsedAst.Declaration.Fun
  }

  def EnumDeclaration: Rule1[ParsedAst.Declaration.Enum] = {
    def UnitCase: Rule1[ParsedAst.Type.Tag] = rule {
      atomic("case") ~ WS ~ Ident ~> ((ident: ParsedAst.Ident) => ParsedAst.Type.Tag(ident, ParsedAst.Type.Unit))
    }

    def NestedCase: Rule1[ParsedAst.Type.Tag] = rule {
      atomic("case") ~ WS ~ Ident ~ Type ~> ParsedAst.Type.Tag
    }

    def Cases: Rule1[Seq[ParsedAst.Type.Tag]] = rule {
      // NB: NestedCase must be parsed before UnitCase.
      oneOrMore(NestedCase | UnitCase).separatedBy(optWS ~ "," ~ optWS)
    }

    rule {
      atomic("enum") ~ WS ~ Ident ~ optWS ~ "{" ~ optWS ~ Cases ~ optWS ~ "}" ~ optSC ~> ParsedAst.Declaration.Enum
    }
  }

  def LatticeDeclaration: Rule1[ParsedAst.Declaration] = {
    def Elms: Rule1[Seq[ParsedAst.QName]] = rule {
      oneOrMore(QName).separatedBy(optWS ~ "," ~ optWS)
    }

    def Traits: Rule1[Seq[ParsedAst.Declaration.Trait]] = rule {
      zeroOrMore(atomic("with") ~ WS ~ Ident ~ optWS ~ "(" ~ QName ~ ")" ~ optWS ~> ParsedAst.Declaration.Trait)
    }

    rule {
      atomic("lat") ~ optWS ~ "<" ~ Ident ~ ">" ~ optWS ~ "(" ~ optWS ~ Elms ~ optWS ~ ")" ~ optWS ~ Traits ~ optSC ~> ParsedAst.Declaration.Lattice
    }
  }

  def RelationDeclaration: Rule1[ParsedAst.Declaration.Relation] = {
    def Attribute: Rule1[(ParsedAst.Ident, ParsedAst.Type)] = rule {
      Ident ~ optWS ~ ":" ~ optWS ~ Type ~> ((ident: ParsedAst.Ident, tpe: ParsedAst.Type) => (ident, tpe))
    }

    def Attributes: Rule1[Seq[(ParsedAst.Ident, ParsedAst.Type)]] = rule {
      oneOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS)
    }

    rule {
      atomic("rel") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ optSC ~> ParsedAst.Declaration.Relation
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
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
      atomic("case") ~ WS ~ Pattern ~ optWS ~ atomic("=>") ~ WS ~ Expression ~ optSC ~> ((p: ParsedAst.Pattern, e: ParsedAst.Expression) => (p, e))
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
  // NB: TagPattern must be before LiteralPattern and VariablePattern.
  def Pattern: Rule1[ParsedAst.Pattern] = rule {
    TagPattern | LiteralPattern | TuplePattern | WildcardPattern | VariablePattern
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

  def TagPattern: Rule1[ParsedAst.Pattern.Tag] = rule {
    QName ~ "." ~ Ident ~ optWS ~ optional(Pattern) ~>
      ((name: ParsedAst.QName, ident: ParsedAst.Ident, pattern: Option[ParsedAst.Pattern]) => pattern match {
        case None => ParsedAst.Pattern.Tag(name, ident, ParsedAst.Pattern.Lit(ParsedAst.Literal.Unit))
        case Some(p) => ParsedAst.Pattern.Tag(name, ident, p)
      })
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
  // NB: ApplyTerm must be parsed before LiteralTerm which must be parsed before VariableTerm.
  def Term: Rule1[ParsedAst.Term] = rule {
    ApplyTerm | LiteralTerm | WildcardTerm | VariableTerm
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
    ParametricType | AmbiguousType | TupleType | LatticeType
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

  def LatticeType: Rule1[ParsedAst.Type.Lattice] = rule {
    "<" ~ Type ~ ">" ~> ParsedAst.Type.Lattice
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
    UnitLiteral | BoolLiteral | IntLiteral | StrLiteral | TagLiteral | TupleLiteral
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

  def TagLiteral: Rule1[ParsedAst.Literal.Tag] = rule {
    QName ~ "." ~ Ident ~ optWS ~ optional(Literal) ~>
      ((name: ParsedAst.QName, ident: ParsedAst.Ident, literal: Option[ParsedAst.Literal]) => literal match {
        case None => ParsedAst.Literal.Tag(name, ident, ParsedAst.Literal.Unit)
        case Some(lit) => ParsedAst.Literal.Tag(name, ident, lit)
      })
  }

  def TupleLiteral: Rule1[ParsedAst.Literal] = {
    def Singleton: Rule1[ParsedAst.Literal] = rule {
      "(" ~ optWS ~ Literal ~ optWS ~ ")"
    }

    def Tuple: Rule1[ParsedAst.Literal] = rule {
      "(" ~ optWS ~ oneOrMore(Literal).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~> ParsedAst.Literal.Tuple
    }

    rule {
      Singleton | Tuple
    }
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
