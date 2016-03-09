package ca.uwaterloo.flix.language.phase

import java.util.zip.ZipFile

import ca.uwaterloo.flix.language.ast.{Type => PType}
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import org.parboiled2._

import scala.collection.immutable.Seq
import scala.io.Source

// TODO: Parse whitespace more "tightly" to improve source positions.
// TODO: Add support for characters.
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
    SP ~ atomic("namespace") ~ WS ~ NName ~ optWS ~ '{' ~ optWS ~ zeroOrMore(Declaration).separatedBy(optWS) ~ optWS ~ '}' ~ SP ~ optSC ~> ParsedAst.Declaration.Namespace
  }

  def Definition: Rule1[ParsedAst.Definition] = rule {
    FunctionDefinition | ExternDefinition | EnumDefinition | BoundedLatticeDefinition | RelationDefinition | LatticeDefinition | IndexDefinition | LawDefinition | ClassDefinition | ImplDefinition
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
    SP ~ atomic("fn") ~ WS ~ Ident ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~ optSC ~> ParsedAst.Definition.Signature
  }

  def ExternDefinition: Rule1[ParsedAst.Definition.External] = rule {
    SP ~ atomic("external") ~ optWS ~ atomic("def") ~ WS ~ Ident ~ optWS ~ FormalParams ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~ optSC ~> ParsedAst.Definition.External
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
  def Expression: Rule1[ParsedAst.Expression] = rule {
    Expressions.Logical
  }

  object Expressions {
    // TODO: The placement of SL is sub optimal for binary expressions.

    def Logical: Rule1[ParsedAst.Expression] = rule {
      Comparison ~ optional(optWS ~ SP ~ Operators.LogicalOp ~ optWS ~ Comparison ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Comparison: Rule1[ParsedAst.Expression] = rule {
      Additive ~ optional(optWS ~ SP ~ Operators.ComparisonOp ~ optWS ~ Additive ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Additive: Rule1[ParsedAst.Expression] = rule {
      Multiplicative ~ zeroOrMore(optWS ~ SP ~ Operators.AdditiveOp ~ optWS ~ Multiplicative ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Multiplicative: Rule1[ParsedAst.Expression] = rule {
      Infix ~ zeroOrMore(optWS ~ SP ~ Operators.MultiplicativeOp ~ optWS ~ Infix ~ SP ~> ParsedAst.Expression.Binary)
    }

    def Infix: Rule1[ParsedAst.Expression] = rule {
      Extended ~ optional(optWS ~ "`" ~ SP ~ QName ~ "`" ~ optWS ~ Extended ~ SP ~> ParsedAst.Expression.Infix)
    }

    def Extended: Rule1[ParsedAst.Expression] = rule {
      Unary ~ optional(optWS ~ SP ~ Operators.ExtendedBinaryOp ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.ExtendedBinary)
    }

    def Unary: Rule1[ParsedAst.Expression] = rule {
      !Literal ~ (SP ~ Operators.UnaryOp ~ optWS ~ Unary ~ SP ~> ParsedAst.Expression.Unary) | Ascribe
    }

    def Ascribe: Rule1[ParsedAst.Expression] = rule {
      SP ~ Invoke ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Expression.Ascribe | Invoke
    }

    def Invoke: Rule1[ParsedAst.Expression] = rule {
      Apply | Simple
    }

    def Simple: Rule1[ParsedAst.Expression] = rule {
      LetMatch | IfThenElse | Switch | Match |
        FNil | FNone | FSome | Tag | Tuple | FSet | FMap |
        Literal | Lambda | Existential | Universal | Bot | Top | Var | UserError
    }

    def Literal: Rule1[ParsedAst.Expression.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Expression.Lit
    }

    def IfThenElse: Rule1[ParsedAst.Expression.IfThenElse] = rule {
      SP ~ atomic("if") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ optWS ~ Expression ~ optWS ~ atomic("else") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.IfThenElse
    }

    def LetMatch: Rule1[ParsedAst.Expression.LetMatch] = rule {
      SP ~ atomic("let") ~ optWS ~ Pattern ~ optWS ~ "=" ~ optWS ~ Expression ~ optWS ~ atomic("in") ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.LetMatch
    }

    def Match: Rule1[ParsedAst.Expression.Match] = {
      def Rule: Rule1[(ParsedAst.Pattern, ParsedAst.Expression)] = rule {
        atomic("case") ~ optWS ~ Pattern ~ optWS ~ atomic("=>") ~ optWS ~ Expression ~ optSC ~> ((p: ParsedAst.Pattern, e: ParsedAst.Expression) => (p, e))
      }

      rule {
        SP ~ atomic("match") ~ optWS ~ Expression ~ optWS ~ atomic("with") ~ optWS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Match
      }
    }

    def Switch: Rule1[ParsedAst.Expression.Switch] = {
      def Rule: Rule1[(ParsedAst.Expression, ParsedAst.Expression)] = rule {
        atomic("case") ~ optWS ~ Expression ~ optWS ~ "=>" ~ optWS ~ Expression ~> ((e1: ParsedAst.Expression, e2: ParsedAst.Expression) => (e1, e2))
      }

      rule {
        SP ~ atomic("switch") ~ optWS ~ "{" ~ optWS ~ oneOrMore(Rule).separatedBy(optWS) ~ optWS ~ "}" ~ SP ~> ParsedAst.Expression.Switch
      }
    }

    def Apply: Rule1[ParsedAst.Expression.Apply] = rule {
      SP ~ Simple ~ optWS ~ "(" ~ optWS ~ zeroOrMore(Expression).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.Apply
    }

    // TODO: Cleanup
    def Tag: Rule1[ParsedAst.Expression.Tag] = rule {
      SP ~ QName ~ "." ~ Ident ~ optional(optWS ~ Tuple) ~ SP ~>
        ((sp1: SourcePosition, name: Name.QName, ident: Name.Ident, exp: Option[ParsedAst.Expression], sp2: SourcePosition) => exp match {
          case None => ParsedAst.Expression.Tag(sp1, name, ident, ParsedAst.Expression.Lit(sp1, ParsedAst.Literal.Unit(sp1, sp2), sp2), sp2)
          case Some(e) => ParsedAst.Expression.Tag(sp1, name, ident, e, sp2)
        })
    }

    // TODO: Cleanup
    def Tuple: Rule1[ParsedAst.Expression] = {
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

    def FNil: Rule1[ParsedAst.Expression.FNil] = rule {
      SP ~ atomic("Nil") ~ SP ~> ParsedAst.Expression.FNil
    }

    def FNone: Rule1[ParsedAst.Expression.FNone] = rule {
      SP ~ atomic("None") ~ SP ~> ParsedAst.Expression.FNone
    }

    def FSome: Rule1[ParsedAst.Expression.FSome] = rule {
      SP ~ atomic("Some") ~ optWS ~ "(" ~ optWS ~ Expression ~ optWS ~ ")" ~ SP ~> ParsedAst.Expression.FSome
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

    // TODO: can we get rid of this
    def Bot: Rule1[ParsedAst.Expression.Bot] = rule {
      SP ~ "⊥" ~ SP ~> ParsedAst.Expression.Bot
    }

    // TODO: can we get rid of this
    def Top: Rule1[ParsedAst.Expression.Top] = rule {
      SP ~ "⊤" ~ SP ~> ParsedAst.Expression.Top
    }

    def Var: Rule1[ParsedAst.Expression.Var] = rule {
      SP ~ QName ~ SP ~> ParsedAst.Expression.Var
    }

    def Lambda: Rule1[ParsedAst.Expression.Lambda] = rule {
      SP ~ atomic("fn") ~ optWS ~ "(" ~ ArgumentList ~ ")" ~ optWS ~ ":" ~ optWS ~ Type ~ optWS ~ "=" ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Lambda
    }

    def Existential: Rule1[ParsedAst.Expression.Existential] = rule {
      SP ~ atomic("∃" | "\\exists") ~ optWS ~ FormalParams ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Existential
    }

    def Universal: Rule1[ParsedAst.Expression.Universal] = rule {
      SP ~ atomic("∀" | "\\forall") ~ optWS ~ FormalParams ~ optWS ~ "." ~ optWS ~ Expression ~ SP ~> ParsedAst.Expression.Universal
    }

    def UserError: Rule1[ParsedAst.Expression] = rule {
      SP ~ atomic("???") ~ optWS ~ ":" ~ optWS ~ Type ~ SP ~> ParsedAst.Expression.UserError
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  // NB: List must be parsed before everything.
  // NB: Literal must be parsed before Variable.
  // NB: Tag must be before Literal and Variable.
  def Pattern: Rule1[ParsedAst.Pattern] = rule {
    Patterns.List
  }

  object Patterns {

    def Simple: Rule1[ParsedAst.Pattern] = rule {
      Patterns.Tag | Patterns.Literal | Patterns.Tuple | Patterns.Wildcard | Patterns.Variable
    }

    def Wildcard: Rule1[ParsedAst.Pattern.Wildcard] = rule {
      SP ~ atomic("_") ~ SP ~> ParsedAst.Pattern.Wildcard
    }

    def Variable: Rule1[ParsedAst.Pattern.Var] = rule {
      SP ~ Ident ~ SP ~> ParsedAst.Pattern.Var
    }

    def Literal: Rule1[ParsedAst.Pattern.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Pattern.Lit
    }

    def Tag: Rule1[ParsedAst.Pattern.Tag] = rule {
      SP ~ QName ~ "." ~ Ident ~ optional(optWS ~ Pattern) ~ SP ~>
        ((sp1: SourcePosition, name: Name.QName, ident: Name.Ident, pattern: Option[ParsedAst.Pattern], sp2: SourcePosition) => pattern match {
          case None => ParsedAst.Pattern.Tag(sp1, name, ident, ParsedAst.Pattern.Lit(sp1, ParsedAst.Literal.Unit(sp1, sp2), sp2), sp2)
          case Some(p) => ParsedAst.Pattern.Tag(sp1, name, ident, p, sp2)
        })
    }

    def Tuple: Rule1[ParsedAst.Pattern.Tuple] = rule {
      SP ~ "(" ~ optWS ~ zeroOrMore(Pattern).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ ")" ~ SP ~> ParsedAst.Pattern.Tuple
    }

    def List: Rule1[ParsedAst.Pattern] = rule {
      Simple ~ optional(SP ~ optWS ~ atomic("::") ~ optWS ~ Pattern ~ SP ~> ParsedAst.Pattern.List)
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts and Rules                                                         //
  /////////////////////////////////////////////////////////////////////////////
  def FactDeclaration: Rule1[ParsedAst.Declaration.Fact] = rule {
    SP ~ Predicate ~ optWS ~ "." ~ SP ~> ParsedAst.Declaration.Fact
  }

  def RuleDeclaration: Rule1[ParsedAst.Declaration.Rule] = rule {
    SP ~ Predicate ~ optWS ~ ":-" ~ optWS ~ oneOrMore(Predicate).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "." ~ SP ~> ParsedAst.Declaration.Rule
  }

  /////////////////////////////////////////////////////////////////////////////
  // Predicates                                                              //
  /////////////////////////////////////////////////////////////////////////////
  def Predicate: Rule1[ParsedAst.Predicate] = rule {
    Predicates.Ambiguous | Predicates.NotEqual | Predicates.Alias | Predicates.Loop
  }

  object Predicates {
    def Ambiguous: Rule1[ParsedAst.Predicate.Ambiguous] = rule {
      SP ~ QName ~ optWS ~ "(" ~ oneOrMore(Term).separatedBy(optWS ~ "," ~ optWS) ~ ")" ~ SP ~> ParsedAst.Predicate.Ambiguous
    }

    def NotEqual: Rule1[ParsedAst.Predicate.NotEqual] = rule {
      SP ~ Ident ~ optWS ~ atomic("!=") ~ optWS ~ Ident ~ SP ~> ParsedAst.Predicate.NotEqual
    }

    def Alias: Rule1[ParsedAst.Predicate.Alias] = rule {
      SP ~ Ident ~ optWS ~ atomic(":=") ~ optWS ~ Term ~ SP ~> ParsedAst.Predicate.Alias
    }

    def Loop: Rule1[ParsedAst.Predicate.Loop] = rule {
      SP ~ Ident ~ optWS ~ atomic("<-") ~ optWS ~ Term ~ SP ~> ParsedAst.Predicate.Loop
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  // NB: InfixTerm must be parsed before SimpleTerm.
  def Term: Rule1[ParsedAst.Term] = rule {
    Terms.Apply | Terms.Tag | Terms.Tuple | Terms.Literal | Terms.Wildcard | Terms.Variable
  }

  object Terms {

    def Wildcard: Rule1[ParsedAst.Term.Wildcard] = rule {
      SP ~ atomic("_") ~ SP ~> ParsedAst.Term.Wildcard
    }

    def Variable: Rule1[ParsedAst.Term.Var] = rule {
      SP ~ Ident ~ SP ~> ParsedAst.Term.Var
    }

    def Literal: Rule1[ParsedAst.Term.Lit] = rule {
      SP ~ Parser.this.Literal ~ SP ~> ParsedAst.Term.Lit
    }

    def Tag: Rule1[ParsedAst.Term.Tag] = rule {
      SP ~ QName ~ "." ~ Ident ~ optional(optWS ~ Tuple) ~ SP ~> ParsedAst.Term.Tag
    }

    def Tuple: Rule1[ParsedAst.Term.Tuple] = rule {
      SP ~ "(" ~ optWS ~ zeroOrMore(Term).separatedBy(CommaSep) ~ optWS ~ ")" ~ SP ~> ParsedAst.Term.Tuple
    }

    def Apply: Rule1[ParsedAst.Term.Apply] = rule {
      SP ~ QName ~ optWS ~ "(" ~ zeroOrMore(Term).separatedBy(CommaSep) ~ optWS ~ ")" ~ SP ~> ParsedAst.Term.Apply
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def Type: Rule1[PType] = rule {
    Types.LambdaType | Types.TupleType | Types.ParametricType | Types.NamedType
  }

  object Types {

    def NamedType: Rule1[PType] = rule {
      QName ~> PType.Unresolved
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

    def ParametricType: Rule1[PType] = rule {
      QName ~ optWS ~ "[" ~ optWS ~ oneOrMore(Type).separatedBy(optWS ~ "," ~ optWS) ~ optWS ~ "]" ~ optWS ~> PType.Parametric
    }
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
  def LegalIdent: Rule1[String] = {
    rule {
      // TODO: Cleanup
      capture((CharPredicate.Alpha | "⊥" | "⊤" | "⊑" | "⊔" | "⊓" | "▽" | "△" | "⊡") ~ zeroOrMore(CharPredicate.AlphaNum | "_" | "$" | "⊥" | "⊑") ~ zeroOrMore("'"))
    }
  }

  def Ident: Rule1[Name.Ident] = rule {
    SP ~ LegalIdent ~ SP ~> Name.Ident
  }

  def NName: Rule1[Name.NName] = rule {
    SP ~ oneOrMore(Ident).separatedBy(".") ~ SP ~>
      ((sp1: SourcePosition, parts: Seq[Name.Ident], sp2: SourcePosition) => Name.NName(sp1, parts.toList, sp2))
  }

  def QName: Rule1[Name.QName] = rule {
    SP ~ optional(NName ~ "/") ~ Ident ~ SP ~>
      ((sp1: SourcePosition, nsOpt: Option[Name.NName], ident: Name.Ident, sp2: SourcePosition) => nsOpt match {
        case None => Name.QName(sp1, Name.NName(sp1, List.empty, sp2), ident, sp2)
        case Some(ns) => Name.QName(sp1, ns, ident, sp2)
      })
  }

  def Annotation: Rule1[ParsedAst.Annotation] = rule {
    SP ~ atomic("@") ~ LegalIdent ~ SP ~> ParsedAst.Annotation
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Literal: Rule1[ParsedAst.Literal] = rule {
    Literals.Bool | Literals.Char | Literals.Float | Literals.Int | Literals.Str
  }

  object Literals {

    def Bool: Rule1[ParsedAst.Literal.Bool] = rule {
      SP ~ capture(atomic("true") | atomic("false")) ~ SP ~> ParsedAst.Literal.Bool
    }

    def Char: Rule1[ParsedAst.Literal.Char] = rule {
      SP ~ "'" ~ capture(!"'" ~ CharPredicate.All) ~ "'" ~ SP ~> ParsedAst.Literal.Char
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
      Int8 | Int16 | Int32 | Int64 | IntDefault
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
  // Operators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  object Operators {

    /**
      * Parses a unary operator.
      */
    def UnaryOp: Rule1[UnaryOperator] = rule {
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
    oneOrMore(" " | "\t" | NewLine | Comment)
  }

  def optWS: Rule0 = rule {
    optional(WS)
  }

  def optSC: Rule0 = rule {
    optional(optWS ~ ";")
  }

  def NewLine: Rule0 = rule {
    "\n" | "\r\n"
  }

  def CommaSep: Rule0 = rule {
    optWS ~ "," ~ optWS
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def Comment: Rule0 = rule {
    Comments.SingleLineComment | Comments.MultiLineComment
  }

  object Comments {
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
