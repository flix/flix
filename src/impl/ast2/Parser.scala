package impl.ast2

import impl.logic.{BinaryOperator, UnaryOperator}
import org.parboiled2._
import scala.collection.immutable.Seq
import scala.util.{Failure, Success}

object Parser {

  def parse(s: String): Ast.Root = {
    val parser = new Parser(s)
    parser.Root.run() match {
      case Success(ast) => ast
      case Failure(e: ParseError) => throw new RuntimeException("Expression is not valid: " + parser.formatError(e))
      case Failure(e) => throw new RuntimeException("Unexpected error during parsing run: " + e)
    }
  }
}

// TODO: Sort everything.

class Parser(val input: ParserInput) extends org.parboiled2.Parser {
  def Root: Rule1[Ast.Root] = rule {
    oneOrMore(Declaration) ~ EOI ~> Ast.Root
  }

  def Declaration: Rule1[Ast.Declaration] = rule {
    NameSpace | TypeDeclaration | VariableDeclaration | ValueDeclaration | FunctionDeclaration | FactDeclaration | RuleDeclaraction
  }

  def NameSpace: Rule1[Ast.Declaration.NameSpace] = rule {
    "namespace" ~ WhiteSpace ~ Name ~ WhiteSpace ~ '{' ~ optional(WhiteSpace) ~ NameSpaceBody ~ optional(WhiteSpace) ~ '}' ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.NameSpace
  }

  def NameSpaceBody: Rule1[Seq[Ast.Declaration]] = rule {
    zeroOrMore(Declaration)
  }

  def TypeDeclaration: Rule1[Ast.Declaration.TypeDecl] = rule {
    "type" ~ WhiteSpace ~ Ident ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Type ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.TypeDecl
  }

  def ValueDeclaration: Rule1[Ast.Declaration.Val] = rule {
    "val" ~ WhiteSpace ~ Ident ~ ":" ~ WhiteSpace ~ Type ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.Val
  }

  def VariableDeclaration: Rule1[Ast.Declaration.Var] = rule {
    "var" ~ WhiteSpace ~ Ident ~ ":" ~ WhiteSpace ~ Type ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.Var
  }

  def FunctionDeclaration: Rule1[Ast.Declaration.Function] = rule {
    zeroOrMore(Annotation) ~ "def" ~ WhiteSpace ~ Ident ~ "(" ~ ArgumentList ~ ")" ~ ":" ~ WhiteSpace ~ Type ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~ ";" ~ optional(WhiteSpace) ~> Ast.Declaration.Function
  }

  def FactDeclaration: Rule1[Ast.Declaration.Fact] = rule {
    "fact" ~ WhiteSpace ~ Ident ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Predicate ~ ";" ~ WhiteSpace ~> Ast.Declaration.Fact
  }

  def RuleDeclaraction: Rule1[Ast.Declaration.Rule] = rule {
    "rule" ~ WhiteSpace ~ Ident ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Predicate ~ WhiteSpace ~ "if" ~ WhiteSpace ~ RuleBody ~ ";" ~ WhiteSpace ~> Ast.Declaration.Rule
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

  // TODO: Lattice Decls, etc.

  def Predicate: Rule1[Ast.Predicate] = rule {
    Term ~ WhiteSpace ~ "<-" ~ WhiteSpace ~ Term ~> Ast.Predicate
  }

  def Term: Rule1[Ast.Term] = rule {
    Name ~ "(" ~ oneOrMore(Term).separatedBy("," ~ WhiteSpace) ~ ")" ~> Ast.Term.Call |
      Name ~> Ast.Term.NameRef |
      Digits ~> Ast.Term.Int
  }


  // TODO: Elimnate or move
  def Digits: Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.Digit))
  }

  def Annotation: Rule1[Ast.Annotation] = rule {
    "@" ~ Ident ~ WhiteSpace ~> Ast.Annotation
  }


  /** *************************************************************************/
  /** Expressions                                                           ***/
  /** *************************************************************************/
  def Expression: Rule1[Ast.Expression] = rule {
    LiteralExp | LetExp | IfThenElseExp | MatchExp | TupleExp | VariableExp | ParenthesisExp | MissingExp | ImpossibleExp
  }

  def LiteralExp: Rule1[Ast.Expression] = rule {
    BoolLitExp | IntLitExp | StrLitExp
  }

  def BoolLitExp: Rule1[Ast.Expression.BoolLit] = rule {
    str("true") ~> (() => Ast.Expression.BoolLit(literal = true)) | str("false") ~> (() => Ast.Expression.BoolLit(literal = false))
  }

  def IntLitExp: Rule1[Ast.Expression.IntLit] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((x: String) => Ast.Expression.IntLit(x.toInt))
  }

  def StrLitExp: Rule1[Ast.Expression.StrLit] = rule {
    "\"" ~ capture(zeroOrMore(!"\"" ~ CharPredicate.Printable)) ~ "\"" ~> Ast.Expression.StrLit
  }

  def UnaryExp: Rule1[Ast.Expression.Unary] = rule {
    UnaryOp ~ optional(WhiteSpace) ~ Expression ~> Ast.Expression.Unary
  }

  //  def BinaryExp: Rule1[Ast.Expression.Binary] = rule {
  //    TODO
  //  }

  def InfixExp: Rule1[Ast.Expression.Infix] = rule {
    UnaryExp ~ WhiteSpace ~ "`" ~ Name ~ "`" ~ WhiteSpace ~ UnaryExp ~> Ast.Expression.Infix
  }

  def LetExp: Rule1[Ast.Expression.Let] = rule {
    "let" ~ WhiteSpace ~ Ident ~ WhiteSpace ~ "=" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "in" ~ WhiteSpace ~ Expression ~> Ast.Expression.Let
  }

  // TODO: Consider if with then?
  def IfThenElseExp: Rule1[Ast.Expression.IfThenElse] = rule {
    "if" ~ WhiteSpace ~ "(" ~ Expression ~ ")" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "else" ~ WhiteSpace ~ Expression ~> Ast.Expression.IfThenElse
  }

  def MatchExp: Rule1[Ast.Expression.Match] = rule {
    "match" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "with" ~ WhiteSpace ~ "{" ~ WhiteSpace ~ oneOrMore(MatchRule) ~ "}" ~> Ast.Expression.Match
  }

  def MatchRule: Rule1[Ast.MatchRule] = rule {
    "case" ~ WhiteSpace ~ Pattern ~ WhiteSpace ~ "=>" ~ WhiteSpace ~ Expression ~ ";" ~ WhiteSpace ~> Ast.MatchRule
  }

  def CallExp: Rule1[Ast.Expression.Call] = rule {
    // TODO: Left recursive...
    Expression ~ "(" ~ zeroOrMore(Expression).separatedBy("," ~ WhiteSpace) ~ ")" ~> Ast.Expression.Call
  }

  def ParenthesisExp: Rule1[Ast.Expression] = rule {
    "(" ~ optional(WhiteSpace) ~ Expression ~ optional(WhiteSpace) ~ "}"
  }

  def TupleExp: Rule1[Ast.Expression.Tuple] = rule {
    "(" ~ oneOrMore(Expression).separatedBy("," ~ optional(WhiteSpace)) ~ ")" ~> Ast.Expression.Tuple
  }

  def VariableExp: Rule1[Ast.Expression.UnresolvedName] = rule {
    Name ~> Ast.Expression.UnresolvedName
  }

  def MissingExp: Rule1[Ast.Expression] = rule {
    str("???") ~> (() => Ast.Expression.Missing)
  }

  def ImpossibleExp: Rule1[Ast.Expression] = rule {
    str("!!!") ~> (() => Ast.Expression.Impossible)
  }

  /** *************************************************************************/
  /** Patterns                                                              ***/
  /** *************************************************************************/
  def Pattern: Rule1[Ast.Pattern] = rule {
    WildcardPattern | VariablePattern | TuplePattern
  }

  def WildcardPattern: Rule1[Ast.Pattern] = rule {
    str("_") ~> (() => Ast.Pattern.Wildcard)
  }

  def VariablePattern: Rule1[Ast.Pattern.Var] = rule {
    Ident ~> Ast.Pattern.Var
  }

  def TuplePattern: Rule1[Ast.Pattern.Tuple] = rule {
    "(" ~ oneOrMore(Pattern).separatedBy("," ~ optional(WhiteSpace)) ~ ")" ~> Ast.Pattern.Tuple
  }

  /** *************************************************************************/
  /** Types                                                                 ***/
  /** *************************************************************************/
  def Type: Rule1[Ast.Type] = rule {
    FunctionType | SimpleType
  }

  def SimpleType: Rule1[Ast.Type] = rule {
    UnitType | BoolType | IntType | StrType | TupleType | SetType | MapType | EnumType | NamedType
  }

  def UnitType: Rule1[Ast.Type] = rule {
    str("Unit") ~> (() => Ast.Type.Unit)
  }

  def BoolType: Rule1[Ast.Type] = rule {
    str("Bool") ~> (() => Ast.Type.Bool)
  }

  def IntType: Rule1[Ast.Type] = rule {
    str("Int") ~> (() => Ast.Type.Int)
  }

  def StrType: Rule1[Ast.Type] = rule {
    str("Str") ~> (() => Ast.Type.Str)
  }

  def TupleType: Rule1[Ast.Type.Tuple] = rule {
    "(" ~ oneOrMore(Type).separatedBy("," ~ WhiteSpace) ~ ")" ~> Ast.Type.Tuple
  }

  def SetType: Rule1[Ast.Type.Set] = rule {
    "Set" ~ "[" ~ Type ~ "]" ~> Ast.Type.Set
  }

  def MapType: Rule1[Ast.Type.Map] = rule {
    "Map" ~ "[" ~ Type ~ "," ~ WhiteSpace ~ Type ~ "]" ~> Ast.Type.Map
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

  def NamedType: Rule1[Ast.Type.NameRef] = rule {
    Name ~> Ast.Type.NameRef
  }

  /** *************************************************************************/
  /** Identifiers and Names                                                 ***/
  /** *************************************************************************/
  def Ident: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum))
  }

  def Name: Rule1[Ast.Name] = rule {
    // Note: QualifiedName must preceede SimpleName to avoid left-recursion.
    QualifiedName | SimpleName
  }

  def SimpleName: Rule1[Ast.Name.Simple] = rule {
    Ident ~> Ast.Name.Simple
  }

  def QualifiedName: Rule1[Ast.Name.Qualified] = rule {
    Ident ~ "." ~ Name ~> Ast.Name.Qualified
  }

  /** *************************************************************************/
  /** Operators                                                             ***/
  /** *************************************************************************/
  def UnaryOp: Rule1[UnaryOperator] = rule {
    str("!") ~> (() => UnaryOperator.Not) |
      str("+") ~> (() => UnaryOperator.UnaryPlus) |
      str("-") ~> (() => UnaryOperator.UnaryMinus)
  }

  def BinaryOp: Rule1[BinaryOperator] = rule {
    str("+") ~> (() => BinaryOperator.Plus) |
      str("-") ~> (() => BinaryOperator.Minus) |
      str("*") ~> (() => BinaryOperator.Times) |
      str("/") ~> (() => BinaryOperator.Divide) |
      str("<=") ~> (() => BinaryOperator.LessEqual) |
      str(">=") ~> (() => BinaryOperator.GreaterEqual) |
      str("<") ~> (() => BinaryOperator.Less) |
      str(">") ~> (() => BinaryOperator.Greater) |
      str("==") ~> (() => BinaryOperator.Equal) |
      str("!=") ~> (() => BinaryOperator.NotEqual) |
      str("&&") ~> (() => BinaryOperator.And) |
      str("||") ~> (() => BinaryOperator.Or)
  }

  /** *************************************************************************/
  /** WhiteSpace                                                            ***/
  /** *************************************************************************/
  def WhiteSpace: Rule0 = rule {
    oneOrMore(" " | "\t" | "\n" | "\r")
  }

}
