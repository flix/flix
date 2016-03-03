package ca.uwaterloo.flix.language.phase

import java.util.zip.ZipFile

import ca.uwaterloo.flix.language.ast.{Type => PType}
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import org.parboiled2._

import scala.collection.immutable.Seq
import scala.io.Source

// TODO: Parse whitespace more "tightly" to improve source positions.
// TODO: Add support for characters.
// TODO: Add pattern matching let* pattern = exp
// TODO: Move components into objects.

/**
  * A parser for the Flix language.
  */
class Parser(val source: SourceInput) extends org.parboiled2.Parser {

  /*
   * Initialize parser input.
   */
  override val input: ParserInput = source match {
    case SourceInput.Str(str) => str
    case SourceInput.TxtFile(path) => Source.fromFile(path.toFile).getLines().mkString("\n") // TODO: Slow
    case SourceInput.ZipFile(path) =>
      val file = new ZipFile(path.toFile)
      val entry = file.entries().nextElement()
      val inputStream = file.getInputStream(entry)
      Source.fromInputStream(inputStream).getLines().mkString("\n") // TODO: Slow
  }

  /////////////////////////////////////////////////////////////////////////////
  // Root                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  def Root: Rule1[ParsedAst.Root] = rule {
    push(System.nanoTime()) ~ optWS ~ zeroOrMore(Declaration).separatedBy(optWS) ~ optWS ~ push(System.nanoTime()) ~ EOI ~>
      ((b: Long, decls: Seq[ParsedAst.Declaration], e: Long) => ParsedAst.Root(decls, Time(e - b, 0, 0, 0, 0)))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations and Definition                                             //
  /////////////////////////////////////////////////////////////////////////////
  // NB: RuleDeclaration must be parsed before FactDeclaration.
  def Declaration: Rule1[ParsedAst.Declaration] = rule {
    NamespaceDeclaration | RuleDeclaration | FactDeclaration | Definition
  }

  def NamespaceDeclaration: Rule1[ParsedAst.Declaration.Namespace] = rule {
    SP ~ atomic("namespace") ~ WS ~ QName ~ optWS ~ '{' ~ optWS ~ zeroOrMore(Declaration).separatedBy(optWS) ~ optWS ~ '}' ~ SP ~ optSC ~> ParsedAst.Declaration.Namespace
  }

  def Definition: Rule1[ParsedAst.Definition] = rule {
    FunctionDefinition | EnumDefinition | BoundedLatticeDefinition | RelationDefinition | LatticeDefinition | IndexDefinition | LawDefinition | ClassDefinition | ImplDefinition
  }

  def FunctionDefinition: Rule1[ParsedAst.Definition.Function] = {
    def Annotations: Rule1[Seq[ParsedAst.Annotation]] = rule {
      zeroOrMore(Annotation).separatedBy(WS)
    }

    rule {
      SP ~ Annotations ~ optWS ~ (atomic("def") | atomic("fn")) ~ WS ~ Ident ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~ optSC ~> ParsedAst.Definition.Function
    }
  }

  def SignatureDefinition: Rule1[ParsedAst.Definition.Signature] = rule {
    SP ~ atomic("fn") ~ WS ~ Ident ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~ optSC~> ParsedAst.Definition.Signature
  }

  def LawDefinition: Rule1[ParsedAst.Definition.Law] = rule {
    SP ~ atomic("law") ~ WS ~ Ident ~ optWS ~ TypeParams ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~ optSC ~> ParsedAst.Definition.Law
  }

  def FormalParams: Rule1[Seq[ParsedAst.FormalArg]] = rule {
    optional("(" ~ optWS ~ ArgumentList ~ optWS ~ ")") ~> ((o: Option[Seq[ParsedAst.FormalArg]]) => o match {
      case None => Seq.empty
      case Some(xs) => xs
    })
  }

  def TypeParams: Rule1[Seq[ParsedAst.ContextBound]] = {
    def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
      SP ~ Ident ~ optional(optWS ~ ":" ~ optWS ~ Type) ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, bound: Option[PType], sp2: SourcePosition) => bound match {
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

  def EnumDefinition: Rule1[ParsedAst.Definition.Enum] = {
    def UnitCase: Rule1[ParsedAst.Case] = rule {
      SP ~ atomic("case") ~ WS ~ Ident ~ SP ~> ((sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) => ParsedAst.Case(sp1, ident, PType.Unit, sp2))
    }

    def NestedCase: Rule1[ParsedAst.Case] = rule {
      SP ~ atomic("case") ~ WS ~ Ident ~ Type ~ SP ~> ParsedAst.Case
    }

    def Cases: Rule1[Seq[ParsedAst.Case]] = rule {
      // NB: NestedCase must be parsed before UnitCase.
      oneOrMore(NestedCase | UnitCase).separatedBy(optWS ~ "," ~ optWS)
    }

    rule {
      SP ~ atomic("enum") ~ WS ~ Ident ~ optWS ~ "{" ~ optWS ~ Cases ~ optWS ~ "}" ~ SP ~ optSC ~> ParsedAst.Definition.Enum
    }
  }

  def BoundedLatticeDefinition: Rule1[ParsedAst.Definition] = {
    def Elms: Rule1[Seq[ParsedAst.Expression]] = rule {
      oneOrMore(Expression).separatedBy(optWS ~ "," ~ optWS)
    }

    rule {
      SP ~ atomic("let") ~ optWS ~ Type ~ atomic("<>") ~ optWS ~ "=" ~ optWS ~ "(" ~ optWS ~ Elms ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Definition.BoundedLattice
    }
  }

  def RelationDefinition: Rule1[ParsedAst.Definition.Relation] = rule {
    SP ~ atomic("rel") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Definition.Relation
  }

  def LatticeDefinition: Rule1[ParsedAst.Definition.Lattice] = rule {
    SP ~ atomic("lat") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ Attributes ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Definition.Lattice
  }

  def ClassDefinition: Rule1[ParsedAst.Definition.Class] = {

    def TypeParams: Rule1[Seq[Type]] = rule {
      "[" ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ "]"
    }

    def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
      SP ~ Ident ~ TypeParams ~ SP ~> ParsedAst.ContextBound
    }

    def ContextBounds: Rule1[Seq[ParsedAst.ContextBound]] = rule {
      optional(optWS ~ atomic("=>") ~ optWS ~ oneOrMore(ContextBound).separatedBy(optWS ~ "," ~ optWS) ~ optWS) ~>
        ((o: Option[Seq[ParsedAst.ContextBound]]) => o match {
          case None => Seq.empty
          case Some(xs) => xs
        })
    }

    def ClassBody: Rule1[Seq[ParsedAst.Definition]] = rule {
      "{" ~ optWS ~ zeroOrMore(FunctionDefinition | SignatureDefinition | LawDefinition).separatedBy(WS) ~ optWS ~ "}"
    }

    rule {
      SP ~ atomic("class") ~ WS ~ Ident ~ TypeParams ~ optWS ~ ContextBounds ~ optWS ~ ClassBody ~ SP ~> ParsedAst.Definition.Class
    }
  }

  def ImplDefinition: Rule1[ParsedAst.Definition.Impl] = {

    def TypeParams: Rule1[Seq[Type]] = rule {
      "[" ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ "]"
    }

    def ContextBound: Rule1[ParsedAst.ContextBound] = rule {
      SP ~ Ident ~ TypeParams ~ SP ~> ParsedAst.ContextBound
    }

    def ContextBounds: Rule1[Seq[ParsedAst.ContextBound]] = rule {
      optional(optWS ~ atomic("<=") ~ optWS ~ oneOrMore(ContextBound).separatedBy(optWS ~ "," ~ optWS) ~ optWS) ~>
        ((o: Option[Seq[ParsedAst.ContextBound]]) => o match {
          case None => Seq.empty
          case Some(xs) => xs
        })
    }

    def ImplBody: Rule1[Seq[ParsedAst.Definition.Function]] = rule {
      "{" ~ optWS ~ zeroOrMore(FunctionDefinition).separatedBy(WS) ~ optWS ~ "}"
    }

    rule {
      SP ~ atomic("impl") ~ WS ~ Ident ~ TypeParams ~ optWS ~ ContextBounds ~ optWS ~ ImplBody ~ SP ~> ParsedAst.Definition.Impl
    }
  }

  def Interpretation: Rule1[ParsedAst.Interpretation] = rule {
    Type ~ "<>" ~> ParsedAst.Interpretation.Lattice | Type ~> ParsedAst.Interpretation.Set
  }

  def Attribute: Rule1[ParsedAst.Attribute] = rule {
    Ident ~ optWS ~ ":" ~ optWS ~ Interpretation ~> ParsedAst.Attribute
  }

  def Attributes: Rule1[Seq[ParsedAst.Attribute]] = rule {
    oneOrMore(Attribute).separatedBy(optWS ~ "," ~ optWS)
  }

  def IndexDefinition: Rule1[ParsedAst.Definition.Index] = {
    def Indexes: Rule1[Seq[Name.Ident]] = rule {
      "{" ~ optWS ~ zeroOrMore(Ident).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}"
    }

    rule {
      SP ~ atomic("index") ~ WS ~ Ident ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Indexes).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~ optSC ~> ParsedAst.Definition.Index
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
    ComparisonExpression ~ optional(optWS ~ SP ~ Operator.LogicalOp ~ optWS ~ ComparisonExpression ~ SP ~> ParsedAst.Expression.Binary)
  }

  def ComparisonExpression: Rule1[ParsedAst.Expression] = rule {
    AdditiveExpression ~ optional(optWS ~ SP ~ Operator.ComparisonOp ~ optWS ~ AdditiveExpression ~ SP ~> ParsedAst.Expression.Binary)
  }

  def AdditiveExpression: Rule1[ParsedAst.Expression] = rule {
    MultiplicativeExpression ~ zeroOrMore(optWS ~ SP ~ Operator.AdditiveOp ~ optWS ~ MultiplicativeExpression ~ SP ~> ParsedAst.Expression.Binary)
  }

  def MultiplicativeExpression: Rule1[ParsedAst.Expression] = rule {
    InfixExpression ~ zeroOrMore(optWS ~ SP ~ Operator.MultiplicativeOp ~ optWS ~ InfixExpression ~ SP ~> ParsedAst.Expression.Binary)
  }

  def InfixExpression: Rule1[ParsedAst.Expression] = rule {
    Extended ~ optional(optWS ~ "`" ~ SP ~ QName ~ "`" ~ optWS ~ Extended ~ SP ~> ParsedAst.Expression.Infix)
  }

  def Extended: Rule1[ParsedAst.Expression] = rule {
    UnaryExpression ~ optional(optWS ~ SP ~ Operator.ExtendedBinaryOp ~ optWS ~ UnaryExpression ~ SP ~> ParsedAst.Expression.ExtendedBinary)
  }

  def UnaryExpression: Rule1[ParsedAst.Expression] = rule {
    (SP ~ Operator.Unary ~ optWS ~ UnaryExpression ~ SP ~> ParsedAst.Expression.Unary) | AscribeExpression
  }

  def AscribeExpression: Rule1[ParsedAst.Expression] = rule {
    SP ~ InvokeExpression ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Expression.Ascribe | InvokeExpression
  }

  def InvokeExpression: Rule1[ParsedAst.Expression] = rule {
    ApplyExpression | SimpleExpression
  }

  def SimpleExpression: Rule1[ParsedAst.Expression] = rule {
    LetExpression | IfThenElseExpression | SwitchExpression | MatchExpression | TagExpression | TupleExpression |
      SetExpression | LiteralExpression | LambdaExpression | ExistentialExpression | UniversalExpression | BotExpression |
      TopExpression | VariableExpression | ErrorExpression
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

  def SwitchExpression: Rule1[ParsedAst.Expression.Switch] = {
    def SwitchRule: Rule1[(ParsedAst.Expression, ParsedAst.Expression)] = rule {
      atomic("case") ~ optWS ~ Expression ~ optWS ~ "=>" ~ optWS ~ Expression ~> ((e1: ParsedAst.Expression, e2: ParsedAst.Expression) => (e1, e2))
    }

    rule {
      SP ~ atomic("switch") ~ optWS ~ "{" ~ optWS ~ oneOrMore(SwitchRule).separatedBy(optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Switch
    }
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

  def SetExpression: Rule1[ParsedAst.Expression.Set] = rule {
    SP ~ "#{" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Set
  }

  def BotExpression: Rule1[ParsedAst.Expression.Bot] = rule {
    SP ~ "⊥" ~ SP ~> ParsedAst.Expression.Bot
  }

  def TopExpression: Rule1[ParsedAst.Expression.Top] = rule {
    SP ~ "⊤" ~ SP ~> ParsedAst.Expression.Top
  }

  def VariableExpression: Rule1[ParsedAst.Expression.Var] = rule {
    SP ~ QName ~ SP ~> ParsedAst.Expression.Var
  }

  def LambdaExpression: Rule1[ParsedAst.Expression.Lambda] = rule {
    SP ~ atomic("fn") ~ optWS ~ "(" ~ ArgumentList ~ ")" ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Lambda
  }

  def ExistentialExpression: Rule1[ParsedAst.Expression.Existential] = rule {
    SP ~ atomic("∃" | "\\exists") ~ optWS ~ FormalParams ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Existential
  }

  def UniversalExpression: Rule1[ParsedAst.Expression.Universal] = rule {
    SP ~ atomic("∀" | "\\forall") ~ optWS ~ FormalParams ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Universal
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

  def TuplePattern: Rule1[ParsedAst.Pattern] = {
    def Unit: Rule1[ParsedAst.Pattern] = rule {
      SP ~ atomic("()") ~ SP ~> ((sp1: SourcePosition, sp2: SourcePosition) => ParsedAst.Pattern.Lit(sp1, ParsedAst.Literal.Unit(sp1, sp2), sp2))
    }

    def Singleton: Rule1[ParsedAst.Pattern] = rule {
      "(" ~ optWS ~ Pattern ~ optWS ~ ")"
    }

    def Tuple: Rule1[ParsedAst.Pattern] = rule {
      SP ~ "(" ~ optWS ~ oneOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Pattern.Tuple
    }

    rule {
      Unit | Singleton | Tuple
    }
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
    FunctionOrRelationPredicate | NotEqualPredicate | AliasPredicate | LoopPredicate
  }

  def FunctionOrRelationPredicate: Rule1[ParsedAst.Predicate.Ambiguous] = rule {
    SP ~ QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Ambiguous
  }

  def NotEqualPredicate: Rule1[ParsedAst.Predicate.NotEqual] = rule {
    SP ~ Ident ~ optWS ~ atomic("!=") ~ optWS ~ Ident ~ SP ~> ParsedAst.Predicate.NotEqual
  }

  def AliasPredicate: Rule1[ParsedAst.Predicate.Alias] = rule {
    SP ~ Ident ~ optWS ~ atomic(":=") ~ optWS ~ Term ~ SP ~> ParsedAst.Predicate.Alias
  }

  def LoopPredicate: Rule1[ParsedAst.Predicate.Loop] = rule {
    SP ~ Ident ~ optWS ~ atomic("<-") ~ optWS ~ Term ~ SP ~> ParsedAst.Predicate.Loop
  }

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // NB: InfixTerm must be parsed before SimpleTerm.
  def Term: Rule1[ParsedAst.Term] = rule {
    InfixTerm | SimpleTerm
  }

  // NB: AscribeTerm must be parsed before BaseTerm.
  def SimpleTerm: Rule1[ParsedAst.Term] = rule {
    AscribeTerm | BaseTerm
  }

  // NB: ApplyTerm must be parsed before LiteralTerm which must be parsed before VariableTerm.
  def BaseTerm: Rule1[ParsedAst.Term] = rule {
    ApplyTerm | ParenTerm | LiteralTerm | WildcardTerm | VariableTerm
  }

  // TODO: Probably unfold singleton tuples.
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
    SP ~ BaseTerm ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Term.Ascribe
  }

  def ApplyTerm: Rule1[ParsedAst.Term.Apply] = rule {
    SP ~ QName ~ optWS ~ "(" ~ zeroOrMore(Term).separatedBy("," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Term.Apply
  }

  def InfixTerm: Rule1[ParsedAst.Term.Infix] = rule {
    SP ~ SimpleTerm ~ optWS ~ "`" ~ QName ~ "`" ~ optWS ~ SimpleTerm ~ SP ~> ParsedAst.Term.Infix
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def Type: Rule1[PType] = rule {
    LambdaType | TupleType | SetType | ParametricType | NativeType | PropType | NamedType
  }

  def NamedType: Rule1[PType] = rule {
    QName ~> PType.Unresolved
  }

  // TODO: Move these later in the pipeline.
  def NativeType: Rule1[PType] = rule {
    atomic("Native") ~> (() => PType.Native)
  }

  def PropType: Rule1[PType] = rule {
    atomic("Prop") ~> (() => PType.Prop)
  }

  def TupleType: Rule1[PType] = {
    def Unit: Rule1[PType] = rule {
      atomic("()") ~ optWS ~> (() => PType.Unit)
    }

    def Singleton: Rule1[PType] = rule {
      "(" ~ optWS ~ Type ~ optWS ~ ")" ~ optWS
    }

    def Tuple: Rule1[PType] = rule {
      "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~> ((xs: Seq[PType]) => PType.Tuple(xs.toList))
    }

    rule {
      Unit | Singleton | Tuple
    }
  }

  def LambdaType: Rule1[PType] = rule {
    "(" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ optWS ~ atomic("->") ~ optWS ~ Type ~> ((xs: Seq[PType], r: PType) => PType.Lambda(xs.toList, r))
  }

  def SetType: Rule1[PType] = rule {
    atomic("Set") ~ optWS ~ "[" ~ optWS ~ Type ~ optWS ~ "]" ~ optWS ~> PType.Set
  }

  def ParametricType: Rule1[PType] = rule {
    QName ~ optWS ~ "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ optWS ~> PType.Parametric
  }

  /////////////////////////////////////////////////////////////////////////////
  // Helpers                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  def ArgumentList: Rule1[Seq[ParsedAst.FormalArg]] = rule {
    zeroOrMore(Argument).separatedBy(optWS ~ "," ~ optWS)
  }

  def Argument: Rule1[ParsedAst.FormalArg] = rule {
    Ident ~ ":" ~ optWS ~ zeroOrMore(Annotation).separatedBy(WS) ~ optWS ~ Type ~> ParsedAst.FormalArg
  }

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  def LegalIdentifier: Rule1[String] = {
    rule {
      // TODO: Cleanup
      capture((CharPredicate.Alpha | "⊥" | "⊤" | "⊑" | "⊔" | "⊓" | "▽" | "△" | "⊡") ~ zeroOrMore(CharPredicate.AlphaNum | "_" | "$" | "⊥" | "⊑" ) ~ zeroOrMore("'"))
    }
  }

  // TODO: Intern strings?
  def Ident: Rule1[Name.Ident] = rule {
    SP ~ LegalIdentifier ~ SP ~> Name.Ident
  }

  def QName: Rule1[Name.Unresolved] = rule {
    SP ~ oneOrMore(LegalIdentifier).separatedBy(atomic("::")) ~ SP ~>
      ((sp1: SourcePosition, parts: Seq[String], sp2: SourcePosition) => Name.Unresolved(sp1, parts.toList, sp2))
  }

  def Annotation: Rule1[ParsedAst.Annotation] = rule {
    SP ~ atomic("@") ~ LegalIdentifier ~ SP ~> ParsedAst.Annotation
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    UnitLiteral | BoolLiteral | CharLiteral | IntLiteral | StrLiteral | TagLiteral | TupleLiteral | SetLiteral
  }

  def UnitLiteral: Rule1[ParsedAst.Literal.Unit] = rule {
    SP ~ atomic("()") ~ SP ~> ParsedAst.Literal.Unit
  }

  def BoolLiteral: Rule1[ParsedAst.Literal.Bool] = rule {
    SP ~ capture(atomic("true") | atomic("false")) ~ SP ~> ParsedAst.Literal.Bool
  }

  def CharLiteral: Rule1[ParsedAst.Literal.Char] = rule {
    SP ~ "'" ~ capture(CharPredicate.AlphaNum) ~ "'" ~ SP ~> ParsedAst.Literal.Char
  }

  def IntLiteral: Rule1[ParsedAst.Literal] = rule {
    Int8Literal | Int16Literal | Int32Literal | Int64Literal | IntNoSuffixLiteral
  }

  def IntNoSuffixLiteral: Rule1[ParsedAst.Literal.Int32] = rule {
    SP ~ capture(optional("-") ~ oneOrMore(CharPredicate.Digit)) ~ SP ~> ParsedAst.Literal.Int32
  }

  def Int8Literal: Rule1[ParsedAst.Literal.Int8] = rule {
    SP ~ capture(optional("-") ~ oneOrMore(CharPredicate.Digit)) ~ atomic("i8") ~ SP ~> ParsedAst.Literal.Int8
  }

  def Int16Literal: Rule1[ParsedAst.Literal.Int16] = rule {
    SP ~ capture(optional("-") ~ oneOrMore(CharPredicate.Digit)) ~ atomic("i16") ~ SP ~> ParsedAst.Literal.Int16
  }

  def Int32Literal: Rule1[ParsedAst.Literal.Int32] = rule {
    SP ~ capture(optional("-") ~ oneOrMore(CharPredicate.Digit)) ~ atomic("i32") ~ SP ~> ParsedAst.Literal.Int32
  }

  def Int64Literal: Rule1[ParsedAst.Literal.Int64] = rule {
    SP ~ capture(optional("-") ~ oneOrMore(CharPredicate.Digit)) ~ atomic("i64") ~ SP ~> ParsedAst.Literal.Int64
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

  def SetLiteral: Rule1[ParsedAst.Literal.Set] = rule {
    SP ~ "#{" ~ optWS ~ zeroOrMore(Literal).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Literal.Set
  }

  /////////////////////////////////////////////////////////////////////////////
  // Operators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  object Operator {

    /**
      * Parses a unary operator.
      */
    def Unary: Rule1[UnaryOperator] = rule {
      atomic("!") ~> (() => UnaryOperator.LogicalNot) |
        atomic("+") ~> (() => UnaryOperator.Plus) |
        atomic("-") ~> (() => UnaryOperator.Minus) |
        atomic("~") ~> (() => UnaryOperator.BitwiseNegate) |
        atomic("¬") ~> (() => UnaryOperator.LogicalNot)
    }

    /**
      * Parses a logical operator.
      */
    def LogicalOp: Rule1[BinaryOperator] = rule {
      atomic("&&") ~> (() => BinaryOperator.LogicalAnd) |
        atomic("||") ~> (() => BinaryOperator.LogicalOr) |
        atomic("&") ~> (() => BinaryOperator.BitwiseAnd) |
        atomic("|") ~> (() => BinaryOperator.BitwiseOr) |
        atomic("==>") ~> (() => BinaryOperator.Implication) |
        atomic("<==>") ~> (() => BinaryOperator.Biconditional) |
        atomic("^") ~> (() => BinaryOperator.BitwiseXor) |
        atomic("<<") ~> (() => BinaryOperator.BitwiseLeftShift) |
        atomic(">>") ~> (() => BinaryOperator.BitwiseRightShift) |
        atomic("∧") ~> (() => BinaryOperator.LogicalAnd) |
        atomic("∨") ~> (() => BinaryOperator.LogicalOr) |
        atomic("→") ~> (() => BinaryOperator.Implication) |
        atomic("↔") ~> (() => BinaryOperator.Biconditional)
    }

    /**
      * Parses a comparison operator.
      */
    def ComparisonOp: Rule1[BinaryOperator] = rule {
      atomic("<=") ~> (() => BinaryOperator.LessEqual) |
        atomic(">=") ~> (() => BinaryOperator.GreaterEqual) |
        atomic("<") ~> (() => BinaryOperator.Less) |
        atomic(">") ~> (() => BinaryOperator.Greater) |
        atomic("==") ~> (() => BinaryOperator.Equal) |
        atomic("!=") ~> (() => BinaryOperator.NotEqual) |
        atomic("≡") ~> (() => BinaryOperator.Equal)
    }

    /**
      * Parses a multiplicative operator.
      */
    def MultiplicativeOp: Rule1[BinaryOperator] = rule {
      atomic("*") ~> (() => BinaryOperator.Times) |
        atomic("/") ~> (() => BinaryOperator.Divide) |
        atomic("%") ~> (() => BinaryOperator.Modulo)
    }

    /**
      * Parses an additive operator.
      */
    def AdditiveOp: Rule1[BinaryOperator] = rule {
      atomic("+") ~> (() => BinaryOperator.Plus) |
        atomic("-") ~> (() => BinaryOperator.Minus)
    }

    /**
      * Parses an extended binary operator.
      */
    def ExtendedBinaryOp: Rule1[ExtendedBinaryOperator] = rule {
      atomic("⊑") ~> (() => ExtendedBinaryOperator.Leq) |
        atomic("⊔") ~> (() => ExtendedBinaryOperator.Lub) |
        atomic("⊓") ~> (() => ExtendedBinaryOperator.Glb) |
        atomic("▽") ~> (() => ExtendedBinaryOperator.Widen) |
        atomic("△") ~> (() => ExtendedBinaryOperator.Narrow)
    }

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
      push(SourcePosition(source, lineNumber, columnNumber, () => input.getLine(lineNumber)))
    }
  }

}
