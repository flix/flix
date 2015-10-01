package ca.uwaterloo.flix.language.phase

import java.nio.file.Path

import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.annotation.Unoptimized

import org.parboiled2._

import scala.collection.immutable.Seq
import scala.io.Source

// TODO: Dealing with whitespace is hard. Figure out a good way.
// TODO:  Allow fields on case objects.

// TODO: Need meta constraint
// true => A(...), B(...) (MUST-HOLD).
// Salary(name, amount) => Employee(name, <<unbound>>)
// false <= Employee(name, _), !Salary(name, _).

// Safety property
// false <= A(...), B(...) (the body must never hold).
//
// always Answer(x).
// never Unsafe(x).

/**
 * A parser for the Flix language.
 */
class Parser(val source: SourceInput) extends org.parboiled2.Parser {

  /*
   * Initialize parser intput.
   */
  override val input: ParserInput = source match {
    case SourceInput.Str(str) => str
    case SourceInput.File(path) => Source.fromFile(path.toFile).getLines().mkString("\n")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Root                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  def Root: Rule1[ParsedAst.Root] = rule {
    optWS ~ zeroOrMore(Declaration) ~ optWS ~ EOI ~> ParsedAst.Root
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations and Definition                                             //
  /////////////////////////////////////////////////////////////////////////////
  // NB: RuleDeclaration must be parsed before FactDeclaration.
  def Declaration: Rule1[ParsedAst.Declaration] = rule {
    NamespaceDeclaration | RuleDeclaration | FactDeclaration | Definition
  }

  def NamespaceDeclaration: Rule1[ParsedAst.Declaration.Namespace] = rule {
    atomic("namespace") ~ WS ~ QName ~ optWS ~ '{' ~ optWS ~ zeroOrMore(Declaration) ~ optWS ~ '}' ~ optSC ~> ParsedAst.Declaration.Namespace
  }

  def Definition: Rule1[ParsedAst.Definition] = rule {
    ValueDefinition | FunctionDefinition | EnumDefinition | LatticeDefinition | RelationDefinition
  }

  def ValueDefinition: Rule1[ParsedAst.Definition.Value] = rule {
    SL ~ atomic("val") ~ WS ~ Ident ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ optSC ~> ParsedAst.Definition.Value
  }

  def FunctionDefinition: Rule1[ParsedAst.Definition.Function] = rule {
    SL ~ atomic("def") ~ WS ~ Ident ~ optWS ~ "(" ~ ArgumentList ~ ")" ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ optSC ~> ParsedAst.Definition.Function
  }

  def EnumDefinition: Rule1[ParsedAst.Definition.Enum] = {
    def UnitCase: Rule1[ParsedAst.Type.Tag] = rule {
      atomic("case") ~ WS ~ Ident ~> ((ident: Name.Ident) => ParsedAst.Type.Tag(ident, ParsedAst.Type.Unit))
    }

    def NestedCase: Rule1[ParsedAst.Type.Tag] = rule {
      atomic("case") ~ WS ~ Ident ~ Type ~> ParsedAst.Type.Tag
    }

    def Cases: Rule1[Seq[ParsedAst.Type.Tag]] = rule {
      // NB: NestedCase must be parsed before UnitCase.
      oneOrMore(NestedCase | UnitCase).separatedBy(optWS ~ "," ~ optWS)
    }

    rule {
      SL ~ atomic("enum") ~ WS ~ Ident ~ optWS ~ "{" ~ optWS ~ Cases ~ optWS ~ "}" ~ optSC ~> ParsedAst.Definition.Enum
    }
  }

  def LatticeDefinition: Rule1[ParsedAst.Definition] = {
    def Elms: Rule1[Seq[ParsedAst.Expression]] = rule {
      oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
    }

    def Traits: Rule1[Seq[ParsedAst.Trait]] = rule {
      zeroOrMore(atomic("with") ~ WS ~ Ident ~ optWS ~ "(" ~ QName ~ ")" ~ optWS ~> ParsedAst.Trait)
    }

    rule {
      SL ~ atomic("lat") ~ optWS ~ Ident ~ atomic("<>") ~ optWS ~ "(" ~ optWS ~ Elms ~ optWS ~ ")" ~ optWS ~ Traits ~ optSC ~> ParsedAst.Definition.Lattice
    }
  }

  def RelationDefinition: Rule1[ParsedAst.Definition.Relation] = {
    def Attribute: Rule1[ParsedAst.Attribute] = rule {
      Ident ~ optWS ~ ":" ~ optWS ~ Type ~> ParsedAst.Attribute
    }

    def Attributes: Rule1[Seq[ParsedAst.Attribute]] = rule {
      oneOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS)
    }

    rule {
      SL ~ atomic("rel") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ optSC ~> ParsedAst.Definition.Relation
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: The placement of SL is sub optimal for binary expressions.
  def Expression: Rule1[ParsedAst.Expression] = rule {
    LogicalExpression
  }

  def LogicalExpression: Rule1[ParsedAst.Expression] = rule {
    ComparisonExpression ~ optional(optWS ~ SL ~ LogicalOp ~ optWS ~ ComparisonExpression ~> ParsedAst.Expression.Binary)
  }

  def ComparisonExpression: Rule1[ParsedAst.Expression] = rule {
    AdditiveExpression ~ optional(optWS ~ SL ~ ComparisonOp ~ optWS ~ AdditiveExpression ~> ParsedAst.Expression.Binary)
  }

  def AdditiveExpression: Rule1[ParsedAst.Expression] = rule {
    MultiplicativeExpression ~ zeroOrMore(optWS ~ SL ~ AdditiveOp ~ optWS ~ MultiplicativeExpression ~> ParsedAst.Expression.Binary)
  }

  def MultiplicativeExpression: Rule1[ParsedAst.Expression] = rule {
    InfixExpression ~ zeroOrMore(optWS ~ SL ~ MultiplicativeOp ~ optWS ~ InfixExpression ~> ParsedAst.Expression.Binary)
  }

  def InfixExpression: Rule1[ParsedAst.Expression] = rule {
    UnaryExpression ~ optional(optWS ~ "`" ~ SL ~ QName ~ "`" ~ optWS ~ UnaryExpression ~> ParsedAst.Expression.Infix)
  }

  def UnaryExpression: Rule1[ParsedAst.Expression] = rule {
    (SL ~ UnaryOp ~ optWS ~ UnaryExpression ~> ParsedAst.Expression.Unary) | AscribeExpression
  }

  def AscribeExpression: Rule1[ParsedAst.Expression] = rule {
    SL ~ InvokeExpression ~ optWS ~ ":" ~ optWS ~ Type ~> ParsedAst.Expression.Ascribe | InvokeExpression
  }

  def InvokeExpression: Rule1[ParsedAst.Expression] = rule {
    ApplyExpression | SimpleExpression
  }

  def SimpleExpression: Rule1[ParsedAst.Expression] = rule {
    LetExpression | IfThenElseExpression | MatchExpression | TagExpression | TupleExpression | LiteralExpression | LambdaExpression | VariableExpression | ErrorExpression
  }

  def LiteralExpression: Rule1[ParsedAst.Expression.Lit] = rule {
    SL ~ Literal ~> ParsedAst.Expression.Lit
  }

  def LetExpression: Rule1[ParsedAst.Expression.Let] = rule {
    SL ~ atomic("let") ~ WS ~ Ident ~ optWS ~ "=" ~ optWS ~ Expression ~ WS ~ atomic("in") ~ WS ~ Expression ~> ParsedAst.Expression.Let
  }

  def IfThenElseExpression: Rule1[ParsedAst.Expression.IfThenElse] = rule {
    SL ~ atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ WS ~ Expression ~ WS ~ atomic("else") ~ WS ~ Expression ~> ParsedAst.Expression.IfThenElse
  }

  def MatchExpression: Rule1[ParsedAst.Expression.Match] = {
    def MatchRule: Rule1[(ParsedAst.Pattern, ParsedAst.Expression)] = rule {
      atomic("case") ~ WS ~ Pattern ~ optWS ~ atomic("=>") ~ WS ~ Expression ~ optSC ~> ((p: ParsedAst.Pattern, e: ParsedAst.Expression) => (p, e))
    }

    rule {
      SL ~ atomic("match") ~ WS ~ Expression ~ WS ~ atomic("with") ~ optWS ~ "{" ~ WS ~ oneOrMore(MatchRule) ~ "}" ~> ParsedAst.Expression.Match
    }
  }

  def ApplyExpression: Rule1[ParsedAst.Expression.Apply] = rule {
    SL ~ SimpleExpression ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~> ParsedAst.Expression.Apply
  }

  def TagExpression: Rule1[ParsedAst.Expression.Tag] = rule {
    SL ~ QName ~ "." ~ Ident ~ optWS ~ optional(Expression) ~>
      ((loc: SourceLocation, name: Name.Unresolved, ident: Name.Ident, exp: Option[ParsedAst.Expression]) => exp match {
        case None => ParsedAst.Expression.Tag(loc, name, ident, ParsedAst.Expression.Lit(loc, ParsedAst.Literal.Unit(loc)))
        case Some(e) => ParsedAst.Expression.Tag(loc, name, ident, e)
      })
  }

  def TupleExpression: Rule1[ParsedAst.Expression] = {
    def Unit: Rule1[ParsedAst.Expression] = rule {
      SL ~ atomic("()") ~> ((loc: SourceLocation) => ParsedAst.Expression.Lit(loc, ParsedAst.Literal.Unit(loc)))
    }

    def Singleton: Rule1[ParsedAst.Expression] = rule {
      "(" ~ optWS ~ Expression ~ optWS ~ ")"
    }

    def Tuple: Rule1[ParsedAst.Expression] = rule {
      SL ~ "(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~> ParsedAst.Expression.Tuple
    }

    rule {
      Unit | Singleton | Tuple
    }
  }

  def VariableExpression: Rule1[ParsedAst.Expression.Var] = rule {
    SL ~ QName ~> ParsedAst.Expression.Var
  }

  def LambdaExpression: Rule1[ParsedAst.Expression.Lambda] = rule {
    SL ~ atomic("fn") ~ optWS ~ "(" ~ ArgumentList ~ ")" ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~> ParsedAst.Expression.Lambda
  }

  def ErrorExpression: Rule1[ParsedAst.Expression] = rule {
    SL ~ atomic("???") ~ optWS ~ ":" ~ optWS ~ Type ~> ParsedAst.Expression.Error
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
    SL ~ atomic("_") ~> ParsedAst.Pattern.Wildcard
  }

  def VariablePattern: Rule1[ParsedAst.Pattern.Var] = rule {
    SL ~ Ident ~> ParsedAst.Pattern.Var
  }

  def LiteralPattern: Rule1[ParsedAst.Pattern.Lit] = rule {
    SL ~ Literal ~> ParsedAst.Pattern.Lit
  }

  def TagPattern: Rule1[ParsedAst.Pattern.Tag] = rule {
    SL ~ QName ~ "." ~ Ident ~ optWS ~ optional(Pattern) ~>
      ((loc: SourceLocation, name: Name.Unresolved, ident: Name.Ident, pattern: Option[ParsedAst.Pattern]) => pattern match {
        case None => ParsedAst.Pattern.Tag(loc, name, ident, ParsedAst.Pattern.Lit(loc, ParsedAst.Literal.Unit(loc)))
        case Some(p) => ParsedAst.Pattern.Tag(loc, name, ident, p)
      })
  }

  def TuplePattern: Rule1[ParsedAst.Pattern.Tuple] = rule {
    SL ~ "(" ~ oneOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~> ParsedAst.Pattern.Tuple
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts and Rules                                                         //
  /////////////////////////////////////////////////////////////////////////////
  def FactDeclaration: Rule1[ParsedAst.Declaration.Fact] = rule {
    Predicate ~ optDotOrSC ~> ParsedAst.Declaration.Fact
  }

  def RuleDeclaration: Rule1[ParsedAst.Declaration.Rule] = rule {
    Predicate ~ optWS ~ ":-" ~ optWS ~ oneOrMore(Predicate).separatedBy(optWS ~ "," ~ optWS) ~ optDotOrSC ~> ParsedAst.Declaration.Rule
  }

  def Predicate: Rule1[ParsedAst.Predicate] = rule {
    SL ~ QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ optWS ~> ParsedAst.Predicate.Unresolved
  }

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def Term: Rule1[ParsedAst.Term] = rule {
    AscribeTerm | SimpleTerm
  }

  // NB: ApplyTerm must be parsed before LiteralTerm which must be parsed before VariableTerm.
  def SimpleTerm: Rule1[ParsedAst.Term] = rule {
    ApplyTerm | ParenTerm | LiteralTerm | WildcardTerm | VariableTerm
  }

  def ParenTerm: Rule1[ParsedAst.Term] = rule {
    "(" ~ optWS ~ Term ~ optWS ~ ")"
  }

  def WildcardTerm: Rule1[ParsedAst.Term] = rule {
    SL ~ atomic("_") ~> ParsedAst.Term.Wildcard
  }

  def VariableTerm: Rule1[ParsedAst.Term.Var] = rule {
    SL ~ Ident ~> ParsedAst.Term.Var
  }

  def LiteralTerm: Rule1[ParsedAst.Term.Lit] = rule {
    SL ~ Literal ~> ParsedAst.Term.Lit
  }

  def AscribeTerm: Rule1[ParsedAst.Term.Ascribe] = rule {
    SL ~ SimpleTerm ~ optWS ~ ":" ~ optWS ~ Type ~> ParsedAst.Term.Ascribe
  }

  def ApplyTerm: Rule1[ParsedAst.Term.Apply] = rule {
    SL ~ QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy("," ~ optWS) ~ ")" ~> ParsedAst.Term.Apply
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // NB: The parser works left-to-right, but the inline code ensures that the
  // function types are right-associative.
  def Type: Rule1[ParsedAst.Type] = rule {
    oneOrMore(SimpleType).separatedBy(optWS ~ "->" ~ optWS) ~> ((types: Seq[ParsedAst.Type]) => types match {
      case xs if xs.size == 1 => xs.head
      case xs => ParsedAst.Type.Function(xs.dropRight(1).toList, xs.last)
    })
  }

  // NB: ParametricType must be parsed before AmbiguousType.
  def SimpleType: Rule1[ParsedAst.Type] = rule {
    ParametricType | AmbiguousType | TupleType
  }

  def AmbiguousType: Rule1[ParsedAst.Type.Var] = rule {
    QName ~> ParsedAst.Type.Var
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
  def ArgumentList: Rule1[Seq[ParsedAst.FormalArg]] = rule {
    zeroOrMore(Argument).separatedBy(optWS ~ "," ~ optWS)
  }

  def Argument: Rule1[ParsedAst.FormalArg] = rule {
    Ident ~ ":" ~ optWS ~ Type ~> ParsedAst.FormalArg
  }

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  def LegalIdentifier: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") ~ zeroOrMore("'"))
  }

  def Ident: Rule1[Name.Ident] = rule {
    SP ~ LegalIdentifier ~ SP ~>
      ((beginSP: SourcePosition, name: String, endSP: SourcePosition) =>
        Name.Ident(name, getSourceLocation(beginSP, endSP)))
  }

  def QName: Rule1[Name.Unresolved] = rule {
    SL ~ oneOrMore(LegalIdentifier).separatedBy(atomic("::")) ~>
      ((location: SourceLocation, parts: Seq[String]) => Name.Unresolved(parts.toList, location))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    UnitLiteral | BoolLiteral | IntLiteral | StrLiteral | TagLiteral | TupleLiteral
  }

  def UnitLiteral: Rule1[ParsedAst.Literal.Unit] = rule {
    SL ~ atomic("()") ~> ParsedAst.Literal.Unit
  }

  def BoolLiteral: Rule1[ParsedAst.Literal.Bool] = rule {
    TrueLiteral | FalseLiteral
  }

  def TrueLiteral: Rule1[ParsedAst.Literal.Bool] = rule {
    SL ~ atomic("true") ~> ((loc: SourceLocation) => ParsedAst.Literal.Bool(loc, lit = true))
  }

  def FalseLiteral: Rule1[ParsedAst.Literal.Bool] = rule {
    SL ~ atomic("false") ~> ((loc: SourceLocation) => ParsedAst.Literal.Bool(loc, lit = false))
  }

  def IntLiteral: Rule1[ParsedAst.Literal.Int] = rule {
    SL ~ capture(oneOrMore(CharPredicate.Digit)) ~> ((loc: SourceLocation, x: String) => ParsedAst.Literal.Int(loc, x.toInt))
  }

  def StrLiteral: Rule1[ParsedAst.Literal.Str] = rule {
    SL ~ "\"" ~ capture(zeroOrMore(!"\"" ~ CharPredicate.Printable)) ~ "\"" ~> ParsedAst.Literal.Str
  }

  def TagLiteral: Rule1[ParsedAst.Literal.Tag] = rule {
    SL ~ QName ~ "." ~ Ident ~ optWS ~ optional(Literal) ~>
      ((loc: SourceLocation, name: Name.Unresolved, ident: Name.Ident, literal: Option[ParsedAst.Literal]) => literal match {
        case None => ParsedAst.Literal.Tag(loc, name, ident, ParsedAst.Literal.Unit(loc))
        case Some(lit) => ParsedAst.Literal.Tag(loc, name, ident, lit)
      })
  }

  def TupleLiteral: Rule1[ParsedAst.Literal] = {
    def Singleton: Rule1[ParsedAst.Literal] = rule {
      "(" ~ optWS ~ Literal ~ optWS ~ ")"
    }

    def Tuple: Rule1[ParsedAst.Literal] = rule {
      SL ~ "(" ~ optWS ~ oneOrMore(Literal).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~> ParsedAst.Literal.Tuple
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

  def optDotOrSC: Rule0 = rule {
    optWS ~ optional("." | ";") ~ optWS
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
  // Source Positions and Locations                                          //
  /////////////////////////////////////////////////////////////////////////////
  @Unoptimized
  def SP: Rule1[SourcePosition] = {
    val position = Position(cursor, input)
    rule {
      push(SourcePosition(position.line, position.column, input.getLine(position.line)))
    }
  }

  @Unoptimized
  def SL: Rule1[SourceLocation] = {
    val position = Position(cursor, input)
    rule {
      push(ast.SourceLocation(source, position.line, position.column, 0, position.column + 20, input.getLine(position.line)))
    }
  }

  private def getSourceLocation(beginSP: SourcePosition, endSP: SourcePosition): SourceLocation =
    SourceLocation(source, beginSP.lineNumber, beginSP.colNumber, endSP.lineNumber, endSP.colNumber, beginSP.line)

}
