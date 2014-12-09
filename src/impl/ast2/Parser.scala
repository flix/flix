package impl.ast2

import impl.logic.{BinaryOperator, UnaryOperator}
import org.parboiled2._
import scala.collection.immutable.Seq
import scala.util.{Failure, Success}

object Parser {

  def parse(s: String): Ast.Root = {
    // todo: deal with error handling
    val parser = new Parser(s)
    parser.Root.run() match {
      case Success(ast) => ast
      case Failure(e: ParseError) => throw new RuntimeException("Expression is not valid: " + parser.formatError(e))
      case Failure(e) => throw new RuntimeException("Unexpected error during parsing run: " + e)
    }
  }
}

// TODO: Sort everything.
// TODO: Deal properly with optional white space.

class Parser(val input: ParserInput) extends org.parboiled2.Parser {

  def Root: Rule1[Ast.Root] = rule {
    optWhiteSpace ~ zeroOrMore(Declaration) ~ optWhiteSpace ~ EOI ~> Ast.Root
  }

  def Declaration: Rule1[Ast.Declaration] = rule {
    NameSpace | TypeDeclaration | VariableDeclaration | ValueDeclaration | FunctionDeclaration | LatticeDeclaration | RuleDeclaraction | FactDeclaration
  }

  def NameSpace: Rule1[Ast.Declaration.NameSpace] = rule {
    "namespace" ~ WhiteSpace ~ Name ~ WhiteSpace ~ '{' ~ optional(WhiteSpace) ~ zeroOrMore(Declaration) ~ optional(WhiteSpace) ~ '}' ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.NameSpace
  }

  def TypeDeclaration: Rule1[Ast.Declaration.Tpe] = rule {
    "type" ~ WhiteSpace ~ Ident ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Type ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.Tpe
  }

  def ValueDeclaration: Rule1[Ast.Declaration.Val] = rule {
    "val" ~ WhiteSpace ~ Ident ~ ":" ~ WhiteSpace ~ Type ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.Val
  }

  def VariableDeclaration: Rule1[Ast.Declaration.Var] = rule {
    "var" ~ WhiteSpace ~ Ident ~ ":" ~ WhiteSpace ~ LatticeType ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.Var
  }

  def FunctionDeclaration: Rule1[Ast.Declaration.Fun] = rule {
    zeroOrMore(Annotation) ~ "def" ~ WhiteSpace ~ Ident ~ "(" ~ ArgumentList ~ ")" ~ ":" ~ WhiteSpace ~ Type ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.Fun
  }

  // TODO: Remove
  def Annotation: Rule1[String] = rule {
    "@" ~ Ident ~ WhiteSpace
  }

  def LatticeDeclaration: Rule1[Ast.Declaration.Lattice] = rule {
    "lat" ~ WhiteSpace ~ Ident ~ WhiteSpace ~ "=" ~ WhiteSpace ~ RecordExp ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Lattice
  }

  def FactDeclaration: Rule1[Ast.Declaration.Fact] = rule {
    "fact" ~ WhiteSpace ~ Predicate ~ ";" ~ WhiteSpace ~> Ast.Declaration.Fact
  }

  def RuleDeclaraction: Rule1[Ast.Declaration.Rule] = rule {
    "rule" ~ WhiteSpace ~ Predicate ~ WhiteSpace ~ "if" ~ WhiteSpace ~ RuleBody ~ ";" ~ WhiteSpace ~> Ast.Declaration.Rule
  }

  def RuleBody: Rule1[Seq[Ast.Predicate]] = rule {
    oneOrMore(Predicate).separatedBy("," ~ WhiteSpace)
  }

  def ArgumentList: Rule1[Seq[(String, Ast.Type)]] = rule {
    zeroOrMore(Argument).separatedBy("," ~ optional(WhiteSpace))
  }

  def Argument: Rule1[(String, Ast.Type)] = rule {
    Ident ~ ":" ~ WhiteSpace ~ Type ~> ((name: String, typ: Ast.Type) => (name, typ))
  }


  // --------------------------------------------------------------------

  def Predicate: Rule1[Ast.Predicate] = rule {
    Ident ~ WhiteSpace ~ Term ~> Ast.Predicate
  }

  def Term: Rule1[Ast.Term] = rule {
    SimpleTerm ~ zeroOrMore(WhiteSpace ~ "->" ~ WhiteSpace ~ SimpleTerm ~> Ast.Term.Map)
  }

  def SimpleTerm: Rule1[Ast.Term] = rule {
    LiteralTerm | ApplyTerm | NameTerm | TupleTerm | SetTerm
  }

  def LiteralTerm: Rule1[Ast.Term.Lit] = rule {
    Literal ~> Ast.Term.Lit
  }

  def ApplyTerm: Rule1[Ast.Term.Apply] = rule {
    Name ~ "(" ~ oneOrMore(SimpleTerm).separatedBy("," ~ WhiteSpace) ~ ")" ~> Ast.Term.Apply
  }

  def NameTerm: Rule1[Ast.Term.Name] = rule {
    Name ~> Ast.Term.Name
  }

  def TupleTerm: Rule1[Ast.Term.Tuple] = rule {
    "(" ~ oneOrMore(SimpleTerm).separatedBy("," ~ WhiteSpace) ~ ")" ~> Ast.Term.Tuple
  }

  def SetTerm: Rule1[Ast.Term.Set] = rule {
    "{" ~ oneOrMore(SimpleTerm).separatedBy("," ~ WhiteSpace) ~ "}" ~> Ast.Term.Set
  }

  // --------------------------------------------------------------------


  // TODO: Remove?
  def Digits: Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.Digit))
  }

  /** *************************************************************************/
  /** Expressions                                                           ***/
  /** *************************************************************************/
  def Expression: Rule1[Ast.Expression] = rule {
    InfixExp
  }

  def InfixExp: Rule1[Ast.Expression] = rule {
    LogicalExp ~ optional(WhiteSpace ~ "`" ~ Name ~ "`" ~ WhiteSpace ~ LogicalExp ~> Ast.Expression.Infix)
  }

  def LogicalExp: Rule1[Ast.Expression] = rule {
    ComparisonExp ~ zeroOrMore(WhiteSpace ~ LogicalOp ~ WhiteSpace ~ ComparisonExp ~> Ast.Expression.Binary)
  }

  def ComparisonExp: Rule1[Ast.Expression] = rule {
    MultiplicativeExp ~ optional(WhiteSpace ~ ComparisonOp ~ WhiteSpace ~ MultiplicativeExp ~> Ast.Expression.Binary)
  }

  def MultiplicativeExp: Rule1[Ast.Expression] = rule {
    AdditiveExp ~ zeroOrMore(WhiteSpace ~ MultiplicativeOp ~ WhiteSpace ~ AdditiveExp ~> Ast.Expression.Binary)
  }

  def AdditiveExp: Rule1[Ast.Expression] = rule {
    SimpleExpression ~ zeroOrMore(WhiteSpace ~ AdditiveOp ~ WhiteSpace ~ SimpleExpression ~> Ast.Expression.Binary)
  }

  def SimpleExpression: Rule1[Ast.Expression] = rule {
    LiteralExp | LetExp | IfThenElseExp | MatchExp | TupleExp | SetExp | VariableExp | ErrorExp
  }

  def LiteralExp: Rule1[Ast.Expression.Lit] = rule {
    Literal ~> Ast.Expression.Lit
  }

  def SetExp: Rule1[Ast.Expression.Set] = rule {
    "Set" ~ "(" ~ oneOrMore(Expression).separatedBy("," ~ optWhiteSpace) ~ ")" ~> Ast.Expression.Set
  }

  def ErrorExp: Rule1[Ast.Expression] = rule {
    str("???") ~> (() => Ast.Expression.Error)
  }

  def LetExp: Rule1[Ast.Expression.Let] = rule {
    "let" ~ WhiteSpace ~ Ident ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "in" ~ WhiteSpace ~ Expression ~> Ast.Expression.Let
  }

  def IfThenElseExp: Rule1[Ast.Expression.IfThenElse] = rule {
    "if" ~ WhiteSpace ~ "(" ~ Expression ~ ")" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "else" ~ WhiteSpace ~ Expression ~> Ast.Expression.IfThenElse
  }

  def MatchExp: Rule1[Ast.Expression.Match] = rule {
    "match" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "with" ~ WhiteSpace ~ "{" ~ WhiteSpace ~ oneOrMore(MatchRule) ~ "}" ~> Ast.Expression.Match
  }

  def MatchRule: Rule1[(Ast.Pattern, Ast.Expression)] = rule {
    "case" ~ WhiteSpace ~ Pattern ~ WhiteSpace ~ "=>" ~ WhiteSpace ~ Expression ~ ";" ~ WhiteSpace ~> ((p: Ast.Pattern, e: Ast.Expression) => (p, e))
  }


  def CallExp: Rule1[Ast.Expression.Call] = rule {
    // TODO: Left recursive...
    Expression ~ "(" ~ zeroOrMore(Expression).separatedBy("," ~ WhiteSpace) ~ ")" ~> Ast.Expression.Call
  }

  // TODO: Tag Exp

  def TupleExp: Rule1[Ast.Expression.Tuple] = rule {
    "(" ~ oneOrMore(Expression).separatedBy("," ~ optional(WhiteSpace)) ~ ")" ~> Ast.Expression.Tuple
  }

  def RecordExp: Rule1[Ast.Expression.Record] = rule {
    "record" ~ WhiteSpace ~ "{" ~ optWhiteSpace ~ zeroOrMore(KeyValue).separatedBy("," ~ optWhiteSpace) ~ optWhiteSpace ~ "}" ~> Ast.Expression.Record
  }

  private def KeyValue: Rule1[(String, Ast.Expression)] = rule {
    Ident ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~> ((k: String, v: Ast.Expression) => (k, v))
  }

  def VariableExp: Rule1[Ast.Expression.AmbiguousName] = rule {
    Name ~> Ast.Expression.AmbiguousName
  }


  /** *************************************************************************/
  /** Patterns                                                              ***/
  /** *************************************************************************/
  // TODO: Literal.


  def Pattern: Rule1[Ast.Pattern] = rule {
    // Note: TaggedPattern must preceede VariablePattern to avoid left-recursion.
    WildcardPattern | TaggedPattern | TuplePattern | VariablePattern
  }

  def WildcardPattern: Rule1[Ast.Pattern] = rule {
    str("_") ~> (() => Ast.Pattern.Wildcard)
  }

  def VariablePattern: Rule1[Ast.Pattern.Var] = rule {
    Ident ~> Ast.Pattern.Var
  }

  def TaggedPattern: Rule1[Ast.Pattern.Tag] = rule {
    Name ~ WhiteSpace ~ Pattern ~> Ast.Pattern.Tag
  }

  def TuplePattern: Rule1[Ast.Pattern.Tuple] = rule {
    "(" ~ oneOrMore(Pattern).separatedBy("," ~ optional(WhiteSpace)) ~ ")" ~> Ast.Pattern.Tuple
  }

  /** *************************************************************************/
  /** Types                                                                 ***/
  /** *************************************************************************/
  // TODO: Cleanup...
  def Type: Rule1[Ast.Type] = rule {
    FunctionType | SimpleType
  }

  def SimpleType: Rule1[Ast.Type] = rule {
    TupleType | SetType | RelType | MapType | EnumType | NameRefType
  }

  def NameRefType: Rule1[Ast.Type.AmbiguousName] = rule {
    Name ~> Ast.Type.AmbiguousName
  }

  def TupleType: Rule1[Ast.Type.Tuple] = rule {
    "(" ~ oneOrMore(Type).separatedBy("," ~ WhiteSpace) ~ ")" ~> Ast.Type.Tuple
  }

  def SetType: Rule1[Ast.Type.Set] = rule {
    "Set" ~ "[" ~ Type ~ "]" ~> Ast.Type.Set
  }

  def RelType: Rule1[Ast.Type.Rel] = rule {
    "Rel" ~ "[" ~ oneOrMore(Type).separatedBy("," ~ optWhiteSpace) ~ "]" ~> Ast.Type.Rel
  }

  def MapType: Rule1[Ast.Type.Map] = rule {
    "Map" ~ "[" ~ oneOrMore(Type).separatedBy("," ~ WhiteSpace) ~ "]" ~> Ast.Type.Map
  }

  def EnumType: Rule1[Ast.Type.Enum] = rule {
    "enum" ~ WhiteSpace ~ "{" ~ WhiteSpace ~ EnumBody ~ WhiteSpace ~ "}" ~> Ast.Type.Enum
  }

  def EnumBody: Rule1[Seq[Ast.Type.Tag]] = rule {
    oneOrMore("case" ~ WhiteSpace ~ Ident ~> Ast.Type.Tag).separatedBy("," ~ WhiteSpace)
  }

  def FunctionType: Rule1[Ast.Type.Function] = rule {
    SimpleType ~ WhiteSpace ~ "->" ~ WhiteSpace ~ Type ~> Ast.Type.Function
  }

  def LatticeType: Rule1[Ast.Type] = rule {
    MapLatticeType
  }

  def MapLatticeType: Rule1[Ast.Type] = rule {
    ProductLatticeType ~ zeroOrMore(WhiteSpace ~ "->" ~ WhiteSpace ~ ProductLatticeType ~> Ast.Type.MapLattice)
  }

  def ProductLatticeType: Rule1[Ast.Type] = rule {
    SetLatticeType ~ zeroOrMore(WhiteSpace ~ "**" ~ WhiteSpace ~ SetLatticeType ~> Ast.Type.ProductLattice)
  }

  def SetLatticeType: Rule1[Ast.Type] = rule {
    NameRefType | ("{" ~ optWhiteSpace ~ LatticeType ~ optWhiteSpace ~ "}" ~> Ast.Type.SetLattice)
  }

  /** *************************************************************************/
  /** Identifiers and Names                                                 ***/
  /** *************************************************************************/
  def Ident: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum))
  }

  def Name: Rule1[Seq[String]] = rule {
    oneOrMore(Ident).separatedBy(".")
  }

  /** *************************************************************************/
  /** Literals                                                              ***/
  /** *************************************************************************/
  def Literal: Rule1[Ast.Literal] = rule {
    BoolLiteral | IntLiteral | StrLiteral
  }

  def BoolLiteral: Rule1[Ast.Literal.Bool] = rule {
    str("true") ~> (() => Ast.Literal.Bool(literal = true)) | str("false") ~> (() => Ast.Literal.Bool(literal = false))
  }

  def IntLiteral: Rule1[Ast.Literal.Int] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((x: String) => Ast.Literal.Int(x.toInt))
  }

  def StrLiteral: Rule1[Ast.Literal.Str] = rule {
    "\"" ~ capture(zeroOrMore(!"\"" ~ CharPredicate.Printable)) ~ "\"" ~> Ast.Literal.Str
  }

  /** *************************************************************************/
  /** Operators                                                             ***/
  /** *************************************************************************/
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

  /** *************************************************************************/
  /** WhiteSpace                                                            ***/
  /** *************************************************************************/
  def WhiteSpace: Rule0 = rule {
    oneOrMore(" " | "\t" | NewLine | SingleLinecomment | MultiLineComment)
  }

  def optWhiteSpace: Rule0 = rule {
    optional(WhiteSpace)
  }

  def NewLine: Rule0 = rule {
    "\n" | "\r\n"
  }

  /** *************************************************************************/
  /** Source Location                                                       ***/
  /** *************************************************************************/
  def Loc = rule {
    // TODO: Need to track line and column.
    push(cursor)
  }

  /** *************************************************************************/
  /** Comments                                                              ***/
  /** *************************************************************************/
  // Note: We must use ANY to match (consume) whatever character which is not a newline.
  // Otherwise the parser makes no progress and loops.
  def SingleLinecomment: Rule0 = rule {
    "//" ~ zeroOrMore(!NewLine ~ ANY) ~ (NewLine | EOI)
  }

  // Note: We must use ANY to match (consume) whatever character which is not a "*/".
  // Otherwise the parser makes no progress and loops.
  def MultiLineComment: Rule0 = rule {
    "/*" ~ zeroOrMore(!"*/" ~ ANY) ~ "*/"
  }

}
