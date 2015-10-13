package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.annotation.Unoptimized

import org.parboiled2._

import scala.collection.immutable.Seq
import scala.io.Source

// TODO: Parse whitespace more "tightly" to improve source positions.

/**
 * A parser for the Flix language.
 */
class Parser(val source: SourceInput) extends org.parboiled2.Parser {

  /*
   * Initialize parser input.
   */
  override val input: ParserInput = source match {
    case SourceInput.Str(str) => str
    case SourceInput.File(path) => Source.fromFile(path.toFile).getLines().mkString("\n")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Root                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  def Root: Rule1[ParsedAst.Root] = rule {
    optWS ~ zeroOrMore(Declaration).separatedBy(optWS) ~ optWS ~ EOI ~> ParsedAst.Root
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations and Definition                                             //
  /////////////////////////////////////////////////////////////////////////////
  // NB: RuleDeclaration must be parsed before FactDeclaration.
  def Declaration: Rule1[ParsedAst.Declaration] = rule {
    NamespaceDeclaration | RuleDeclaration | FactDeclaration | Definition | Directive
  }

  def NamespaceDeclaration: Rule1[ParsedAst.Declaration.Namespace] = rule {
    SP ~ atomic("namespace") ~ WS ~ QName ~ optWS ~ '{' ~ optWS ~ zeroOrMore(Declaration).separatedBy(optWS) ~ optWS ~ '}' ~ SP ~ optSC ~> ParsedAst.Declaration.Namespace
  }

  def Definition: Rule1[ParsedAst.Definition] = rule {
    ValueDefinition | FunctionDefinition | EnumDefinition | LatticeDefinition | RelationDefinition
  }

  def ValueDefinition: Rule1[ParsedAst.Definition.Value] = rule {
    SP ~ atomic("val") ~ WS ~ Ident ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~ optSC ~> ParsedAst.Definition.Value
  }

  def FunctionDefinition: Rule1[ParsedAst.Definition.Function] = rule {
    SP ~ atomic("def") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ ArgumentList ~ optWS ~ ")" ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~ optSC ~> ParsedAst.Definition.Function
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
      SP ~ atomic("enum") ~ WS ~ Ident ~ optWS ~ "{" ~ optWS ~ Cases ~ optWS ~ "}" ~ SP ~ optSC ~> ParsedAst.Definition.Enum
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
      SP ~ atomic("lat") ~ optWS ~ Type ~ atomic("<>") ~ optWS ~ "(" ~ optWS ~ Elms ~ optWS ~ ")" ~ optWS ~ Traits ~ SP ~ optSC ~> ParsedAst.Definition.Lattice
    }
  }

  def RelationDefinition: Rule1[ParsedAst.Definition.Relation] = {

    def Interpretation: Rule1[ParsedAst.Interpretation] = rule {
      Type ~ "<>" ~> ParsedAst.Interpretation.Lattice | Type ~> ParsedAst.Interpretation.Set
    }

    def Attribute: Rule1[ParsedAst.Attribute] = rule {
      Ident ~ optWS ~ ":" ~ optWS ~ Interpretation ~> ParsedAst.Attribute
    }

    def Attributes: Rule1[Seq[ParsedAst.Attribute]] = rule {
      oneOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS)
    }

    rule {
      SP ~ atomic("rel") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Definition.Relation
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Directives                                                              //
  /////////////////////////////////////////////////////////////////////////////
  // NB: AssertRuleDirective must be parsed before AssertFactDirective.
  def Directive: Rule1[ParsedAst.Directive] = rule {
    AssertRuleDirective | AssertFactDirective | PrintDirective
  }

  def AssertFactDirective: Rule1[ParsedAst.Directive.AssertFact] = rule {
    SP ~ atomic("assert") ~ WS ~ FactDeclaration ~ SP ~> ParsedAst.Directive.AssertFact
  }

  def AssertRuleDirective: Rule1[ParsedAst.Directive.AssertRule] = rule {
    SP ~ atomic("assert") ~ WS ~ RuleDeclaration ~ SP ~> ParsedAst.Directive.AssertRule
  }

  def PrintDirective: Rule1[ParsedAst.Directive.Print] = rule {
    SP ~ atomic("print") ~ WS ~ QName ~ optDotOrSC ~ SP ~> ParsedAst.Directive.Print
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: The placement of SL is sub optimal for binary expressions.
  def Expression: Rule1[ParsedAst.Expression] = rule {
    LogicalExpression
  }

  def LogicalExpression: Rule1[ParsedAst.Expression] = rule {
    ComparisonExpression ~ optional(optWS ~ SP ~ LogicalOp ~ optWS ~ ComparisonExpression ~ SP ~> ParsedAst.Expression.Binary)
  }

  def ComparisonExpression: Rule1[ParsedAst.Expression] = rule {
    AdditiveExpression ~ optional(optWS ~ SP ~ ComparisonOp ~ optWS ~ AdditiveExpression ~ SP ~> ParsedAst.Expression.Binary)
  }

  def AdditiveExpression: Rule1[ParsedAst.Expression] = rule {
    MultiplicativeExpression ~ zeroOrMore(optWS ~ SP ~ AdditiveOp ~ optWS ~ MultiplicativeExpression ~ SP ~> ParsedAst.Expression.Binary)
  }

  def MultiplicativeExpression: Rule1[ParsedAst.Expression] = rule {
    InfixExpression ~ zeroOrMore(optWS ~ SP ~ MultiplicativeOp ~ optWS ~ InfixExpression ~ SP ~> ParsedAst.Expression.Binary)
  }

  def InfixExpression: Rule1[ParsedAst.Expression] = rule {
    UnaryExpression ~ optional(optWS ~ "`" ~ SP ~ QName ~ "`" ~ optWS ~ UnaryExpression ~ SP ~> ParsedAst.Expression.Infix)
  }

  def UnaryExpression: Rule1[ParsedAst.Expression] = rule {
    (SP ~ UnaryOp ~ optWS ~ UnaryExpression ~ SP ~> ParsedAst.Expression.Unary) | AscribeExpression
  }

  def AscribeExpression: Rule1[ParsedAst.Expression] = rule {
    SP ~ InvokeExpression ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Expression.Ascribe | InvokeExpression
  }

  def InvokeExpression: Rule1[ParsedAst.Expression] = rule {
    ApplyExpression | SimpleExpression
  }

  def SimpleExpression: Rule1[ParsedAst.Expression] = rule {
    LetExpression | IfThenElseExpression | MatchExpression | TagExpression | TupleExpression | LiteralExpression | LambdaExpression | VariableExpression | ErrorExpression
  }

  def LiteralExpression: Rule1[ParsedAst.Expression.Lit] = rule {
    SP ~ Literal ~ SP ~> ParsedAst.Expression.Lit
  }

  def LetExpression: Rule1[ParsedAst.Expression.Let] = rule {
    SP ~ atomic("let") ~ optWS ~ Ident ~ optWS ~ "=" ~ optWS ~ Expression ~ optWS ~ atomic("in") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Let
  }

  def IfThenElseExpression: Rule1[ParsedAst.Expression.IfThenElse] = rule {
    SP ~ atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ optWS ~ atomic("else") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
  }

  def MatchExpression: Rule1[ParsedAst.Expression.Match] = {
    def MatchRule: Rule1[(ParsedAst.Pattern, ParsedAst.Expression)] = rule {
      atomic("case") ~ optWS ~ Pattern ~ optWS ~ atomic("=>") ~ optWS ~ Expression ~ optSC ~> ((p: ParsedAst.Pattern, e: ParsedAst.Expression) => (p, e))
    }

    rule {
      SP ~ atomic("match") ~ optWS ~ Expression ~ optWS ~ atomic("with") ~ optWS ~ "{" ~ optWS ~ oneOrMore(MatchRule).separatedBy(optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Match
    }
  }

  def ApplyExpression: Rule1[ParsedAst.Expression.Apply] = rule {
    SP ~ SimpleExpression ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Apply
  }

  def TagExpression: Rule1[ParsedAst.Expression.Tag] = rule {
    SP ~ QName ~ "." ~ Ident ~ optional(optWS ~ TupleExpression) ~ SP ~>
      ((sp1: SourcePosition, name: Name.Unresolved, ident: Name.Ident, exp: Option[ParsedAst.Expression], sp2: SourcePosition) => exp match {
        case None => ParsedAst.Expression.Tag(sp1, name, ident, ParsedAst.Expression.Lit(sp1, ParsedAst.Literal.Unit(sp1, sp2), sp2), sp2)
        case Some(e) => ParsedAst.Expression.Tag(sp1, name, ident, e, sp2)
      })
  }

  def TupleExpression: Rule1[ParsedAst.Expression] = {
    def Unit: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("()") ~ SP ~> ((sp1: SourcePosition, sp2: SourcePosition) => ParsedAst.Expression.Lit(sp1, ParsedAst.Literal.Unit(sp1, sp2), sp2))
    }

    def Singleton: Rule1[ParsedAst.Expression] = rule {
      "(" ~ optWS ~ Expression ~ optWS ~ ")"
    }

    def Tuple: Rule1[ParsedAst.Expression] = rule {
      SP ~ "(" ~ optWS ~ oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Tuple
    }

    rule {
      Unit | Singleton | Tuple
    }
  }

  def VariableExpression: Rule1[ParsedAst.Expression.Var] = rule {
    SP ~ QName ~ SP ~> ParsedAst.Expression.Var
  }

  def LambdaExpression: Rule1[ParsedAst.Expression.Lambda] = rule {
    SP ~ atomic("fn") ~ optWS ~ "(" ~ ArgumentList ~ ")" ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Lambda
  }

  def ErrorExpression: Rule1[ParsedAst.Expression] = rule {
    SP ~ atomic("???") ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Expression.Error
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
    SP ~ atomic("_") ~ SP ~> ParsedAst.Pattern.Wildcard
  }

  def VariablePattern: Rule1[ParsedAst.Pattern.Var] = rule {
    SP ~ Ident ~ SP ~> ParsedAst.Pattern.Var
  }

  def LiteralPattern: Rule1[ParsedAst.Pattern.Lit] = rule {
    SP ~ Literal ~ SP ~> ParsedAst.Pattern.Lit
  }

  def TagPattern: Rule1[ParsedAst.Pattern.Tag] = rule {
    SP ~ QName ~ "." ~ Ident ~ optional(optWS ~ Pattern) ~ SP ~>
      ((sp1: SourcePosition, name: Name.Unresolved, ident: Name.Ident, pattern: Option[ParsedAst.Pattern], sp2: SourcePosition) => pattern match {
        case None => ParsedAst.Pattern.Tag(sp1, name, ident, ParsedAst.Pattern.Lit(sp1, ParsedAst.Literal.Unit(sp1, sp2), sp2), sp2)
        case Some(p) => ParsedAst.Pattern.Tag(sp1, name, ident, p, sp2)
      })
  }

  def TuplePattern: Rule1[ParsedAst.Pattern.Tuple] = rule {
    SP ~ "(" ~ oneOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Pattern.Tuple
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts and Rules                                                         //
  /////////////////////////////////////////////////////////////////////////////
  def FactDeclaration: Rule1[ParsedAst.Declaration.Fact] = rule {
    SP ~ Predicate ~ SP ~ optDotOrSC ~> ParsedAst.Declaration.Fact
  }

  def RuleDeclaration: Rule1[ParsedAst.Declaration.Rule] = rule {
    SP ~ Predicate ~ optWS ~ ":-" ~ optWS ~ oneOrMore(Predicate).separatedBy(optWS ~ "," ~ optWS) ~ SP ~ optDotOrSC ~> ParsedAst.Declaration.Rule
  }

  def Predicate: Rule1[ParsedAst.Predicate] = rule {
    FunctionOrRelationPredicate | NotEqualPredicate | PrintPredicate | ReadPredicate | WritePredicate | ErrorPredicate | AliasPredicate
  }

  def FunctionOrRelationPredicate: Rule1[ParsedAst.Predicate.FunctionOrRelation] = rule {
    SP ~ QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.FunctionOrRelation
  }

  def NotEqualPredicate: Rule1[ParsedAst.Predicate.NotEqual] = rule {
    SP ~ Ident ~ optWS ~ atomic("!=") ~ optWS ~ Ident ~ SP ~> ParsedAst.Predicate.NotEqual
  }

  def PrintPredicate: Rule1[ParsedAst.Predicate.Print] = rule {
    SP ~ atomic("Print#") ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Print
  }

  def ReadPredicate: Rule1[ParsedAst.Predicate.Read] = rule {
    SP ~ atomic("Read#") ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Read
  }

  def WritePredicate: Rule1[ParsedAst.Predicate.Write] = rule {
    SP ~ atomic("Write#") ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Write
  }

  def ErrorPredicate: Rule1[ParsedAst.Predicate.Error] = rule {
    SP ~ atomic("Error#") ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Error
  }

  def AliasPredicate: Rule1[ParsedAst.Predicate.Alias] = rule {
    SP ~ Ident ~ optWS ~ atomic(":=") ~ optWS ~ Term ~ SP ~> ParsedAst.Predicate.Alias
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
    SP ~ atomic("_") ~ SP ~> ParsedAst.Term.Wildcard
  }

  def VariableTerm: Rule1[ParsedAst.Term.Var] = rule {
    SP ~ Ident ~ SP ~> ParsedAst.Term.Var
  }

  def LiteralTerm: Rule1[ParsedAst.Term.Lit] = rule {
    SP ~ Literal ~ SP ~> ParsedAst.Term.Lit
  }

  def AscribeTerm: Rule1[ParsedAst.Term.Ascribe] = rule {
    SP ~ SimpleTerm ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Term.Ascribe
  }

  def ApplyTerm: Rule1[ParsedAst.Term.Apply] = rule {
    SP ~ QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy("," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Term.Apply
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

  def AmbiguousType: Rule1[ParsedAst.Type.Ref] = rule {
    QName ~> ParsedAst.Type.Ref
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

  /////////////////////////////////////////////////////////////////////////////
  // Helpers                                                                 //
  /////////////////////////////////////////////////////////////////////////////
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
    SP ~ LegalIdentifier ~ SP ~> Name.Ident
  }

  def QName: Rule1[Name.Unresolved] = rule {
    SP ~ oneOrMore(LegalIdentifier).separatedBy(atomic("::")) ~ SP ~>
      ((sp1: SourcePosition, parts: Seq[String], sp2: SourcePosition) => Name.Unresolved(sp1, parts.toList, sp2))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    UnitLiteral | BoolLiteral | IntLiteral | StrLiteral | TagLiteral | TupleLiteral
  }

  def UnitLiteral: Rule1[ParsedAst.Literal.Unit] = rule {
    SP ~ atomic("()") ~ SP ~> ParsedAst.Literal.Unit
  }

  def BoolLiteral: Rule1[ParsedAst.Literal.Bool] = rule {
    SP ~ capture(atomic("true") | atomic("false")) ~ SP ~> ParsedAst.Literal.Bool
  }

  def IntLiteral: Rule1[ParsedAst.Literal.Int] = rule {
    SP ~ capture(oneOrMore(CharPredicate.Digit)) ~ SP ~> ParsedAst.Literal.Int
  }

  def StrLiteral: Rule1[ParsedAst.Literal.Str] = rule {
    SP ~ "\"" ~ capture(zeroOrMore(!"\"" ~ CharPredicate.Printable)) ~ "\"" ~ SP ~> ParsedAst.Literal.Str
  }

  def TagLiteral: Rule1[ParsedAst.Literal.Tag] = rule {
    SP ~ QName ~ "." ~ Ident ~ optional(optWS ~ TupleLiteral) ~ SP ~>
      ((sp1: SourcePosition, name: Name.Unresolved, ident: Name.Ident, literal: Option[ParsedAst.Literal], sp2: SourcePosition) => literal match {
        case None => ParsedAst.Literal.Tag(sp1, name, ident, ParsedAst.Literal.Unit(sp1, sp2), sp2)
        case Some(lit) => ParsedAst.Literal.Tag(sp1, name, ident, lit, sp2)
      })
  }

  def TupleLiteral: Rule1[ParsedAst.Literal] = {
    def Singleton: Rule1[ParsedAst.Literal] = rule {
      "(" ~ optWS ~ Literal ~ optWS ~ ")"
    }

    def Tuple: Rule1[ParsedAst.Literal] = rule {
      SP ~ "(" ~ optWS ~ oneOrMore(Literal).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Literal.Tuple
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
    optional(optWS ~ ";")
  }

  def optDotOrSC: Rule0 = rule {
     optional(optWS ~ "." | ";")
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
  // Source Positions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  @Unoptimized
  def SP: Rule1[SourcePosition] = {
    val position = Position(cursor, input)
    val line: String = input.getLine(position.line)
    rule {
      push(SourcePosition(source, position.line, position.column, line))
    }
  }

}
