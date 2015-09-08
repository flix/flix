package ca.uwaterloo.flix.lang

import java.nio.file.{Files, Path}

import ca.uwaterloo.flix.lang
import ca.uwaterloo.flix.lang.ast.Ast
import ca.uwaterloo.flix.util.misc.Unoptimized
import impl.logic.{BinaryOperator, UnaryOperator}
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
      case (ast1, ast2) => Ast.Root(ast1.decls ++ ast2.decls)
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

/**
 * A PEG parser for the Flix programming language.
 */
class Parser(val path: Option[Path], val input: ParserInput) extends org.parboiled2.Parser {

  // TODO: Use atomic keyword

  def Root: Rule1[Ast.Root] = rule {
    optWhiteSpace ~ zeroOrMore(Declaration) ~ optWhiteSpace ~ EOI ~> Ast.Root
  }

  def Declaration: Rule1[Ast.Declaration] = rule {
    NameSpace | TypeDeclaration | VariableDeclaration | ValueDeclaration | FunctionDeclaration | EnumDeclaraction | LatticeDeclaration | RuleDeclaraction | FactDeclaration
  }

  def NameSpace: Rule1[Ast.Declaration.NameSpace] = rule {
    "namespace" ~ WhiteSpace ~ Name ~ optWhiteSpace ~ '{' ~ optWhiteSpace ~ zeroOrMore(Declaration) ~ optWhiteSpace ~ '}' ~ ";" ~ optWhiteSpace ~> Ast.Declaration.NameSpace
  }

  def TypeDeclaration: Rule1[Ast.Declaration.Tpe] = rule {
    "type" ~ WhiteSpace ~ Ident ~ optWhiteSpace ~ "=" ~ optWhiteSpace ~ Type ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Tpe
  }

  def ValueDeclaration: Rule1[Ast.Declaration.Val] = rule {
    "val" ~ WhiteSpace ~ Ident ~ ":" ~ optWhiteSpace ~ Type ~ optWhiteSpace ~ "=" ~ optWhiteSpace ~ Expression ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Val
  }

  def VariableDeclaration: Rule1[Ast.Declaration.Var] = rule {
    "var" ~ WhiteSpace ~ Ident ~ ":" ~ optWhiteSpace ~ Type ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Var
  }

  def FunctionDeclaration: Rule1[Ast.Declaration.Fun] = rule {
    zeroOrMore(Annotation) ~ "def" ~ WhiteSpace ~ Ident ~ "(" ~ ArgumentList ~ ")" ~ ":" ~ optWhiteSpace ~ Type ~ optWhiteSpace ~ "=" ~ optWhiteSpace ~ Expression ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Fun
  }

  def EnumDeclaraction: Rule1[Ast.Declaration.Enum] = rule {
    "enum" ~ WhiteSpace ~ Ident ~ optWhiteSpace ~ "{" ~ optWhiteSpace ~ EnumBody ~ optWhiteSpace ~ "}" ~ optWhiteSpace ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Enum
  }

  def EnumBody: Rule1[Seq[Ast.Type.Tag]] = rule {
    oneOrMore("case" ~ WhiteSpace ~ Ident ~> Ast.Type.Tag).separatedBy("," ~ optWhiteSpace)
  }

  // TODO: Use separate thing for tags.

  def LatticeDeclaration: Rule1[Ast.Declaration.Lattice] = rule {
    "lat" ~ WhiteSpace ~ Ident ~ optWhiteSpace ~ "=" ~ optWhiteSpace ~ RecordExp ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Lattice
  }

  def FactDeclaration: Rule1[Ast.Declaration.Fact] = rule {
    "fact" ~ WhiteSpace ~ Predicate ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Fact
  }

  def RuleDeclaraction: Rule1[Ast.Declaration.Rule] = rule {
    "rule" ~ WhiteSpace ~ Predicate ~ WhiteSpace ~ "if" ~ WhiteSpace ~ RuleBody ~ ";" ~ optWhiteSpace ~> Ast.Declaration.Rule
  }

  def RuleBody: Rule1[Seq[Ast.Predicate]] = rule {
    oneOrMore(Predicate).separatedBy("," ~ optWhiteSpace)
  }

  /** *************************************************************************/
  /** Predicates                                                            ***/
  /** *************************************************************************/

  def Predicate: Rule1[Ast.Predicate] = rule {
    Ident ~ WhiteSpace ~ Term ~> Ast.Predicate
  }

  def Term: Rule1[Ast.Term] = rule {
    SimpleTerm ~ zeroOrMore(WhiteSpace ~ "->" ~ optWhiteSpace ~ SimpleTerm ~> Ast.Term.Map)
  }

  def SimpleTerm: Rule1[Ast.Term] = rule {
    LiteralTerm | ApplyTerm | NameTerm | TupleTerm | SetTerm
  }

  def LiteralTerm: Rule1[Ast.Term.Lit] = rule {
    Literal ~> Ast.Term.Lit
  }

  def ApplyTerm: Rule1[Ast.Term.AmbiguousCall] = rule {
    Name ~ "(" ~ oneOrMore(SimpleTerm).separatedBy("," ~ optWhiteSpace) ~ ")" ~> Ast.Term.AmbiguousCall
  }

  def NameTerm: Rule1[Ast.Term.AmbiguousName] = rule {
    Name ~> Ast.Term.AmbiguousName
  }

  def TupleTerm: Rule1[Ast.Term.Tuple] = rule {
    "(" ~ oneOrMore(SimpleTerm).separatedBy("," ~ optWhiteSpace) ~ ")" ~> Ast.Term.Tuple
  }

  def SetTerm: Rule1[Ast.Term.Set] = rule {
    "{" ~ oneOrMore(SimpleTerm).separatedBy("," ~ optWhiteSpace) ~ "}" ~> Ast.Term.Set
  }

  /** *************************************************************************/
  /** Expressions                                                           ***/
  /** *************************************************************************/
  def Expression: Rule1[Ast.Expression] = rule {
    InfixExp
  }

  def InfixExp: Rule1[Ast.Expression] = rule {
    LogicalExp ~ optional(optWhiteSpace ~ "`" ~ Name ~ "`" ~ optWhiteSpace ~ LogicalExp ~> Ast.Expression.Infix)
  }

  def LogicalExp: Rule1[Ast.Expression] = rule {
    ComparisonExp ~ zeroOrMore(optWhiteSpace ~ LogicalOp ~ optWhiteSpace ~ ComparisonExp ~> Ast.Expression.Binary)
  }

  def ComparisonExp: Rule1[Ast.Expression] = rule {
    MultiplicativeExp ~ optional(optWhiteSpace ~ ComparisonOp ~ optWhiteSpace ~ MultiplicativeExp ~> Ast.Expression.Binary)
  }

  def MultiplicativeExp: Rule1[Ast.Expression] = rule {
    AdditiveExp ~ zeroOrMore(optWhiteSpace ~ MultiplicativeOp ~ optWhiteSpace ~ AdditiveExp ~> Ast.Expression.Binary)
  }

  def AdditiveExp: Rule1[Ast.Expression] = rule {
    SimpleExpression ~ zeroOrMore(optWhiteSpace ~ AdditiveOp ~ optWhiteSpace ~ SimpleExpression ~> Ast.Expression.Binary)
  }

  def SimpleExpression: Rule1[Ast.Expression] = rule {
    LiteralExp | LetExp | IfThenElseExp | MatchExp | TupleExp | MapExp | SetExp | LambdaExp | CallExp | VariableExp | ErrorExp
  }

  def LiteralExp: Rule1[Ast.Expression.Lit] = rule {
    Literal ~> Ast.Expression.Lit
  }

  def SetExp: Rule1[Ast.Expression.Set] = rule {
    "{" ~ optWhiteSpace ~ oneOrMore(Expression).separatedBy("," ~ optWhiteSpace) ~ optWhiteSpace ~ "}" ~> Ast.Expression.Set
  }

  def MapExp: Rule1[Ast.Expression.Map] = rule {
    "{" ~ optWhiteSpace ~ oneOrMore(MapKeyValue).separatedBy("," ~ optWhiteSpace) ~ optWhiteSpace ~ "}" ~> Ast.Expression.Map
  }

  private def MapKeyValue: Rule1[(Ast.Expression, Ast.Expression)] = rule {
    Expression ~ optWhiteSpace ~ "->" ~ optWhiteSpace ~ Expression ~> ((e1: Ast.Expression, e2: Ast.Expression) => (e1, e2))
  }

  def ErrorExp: Rule1[Ast.Expression] = rule {
    str("???") ~> (() => Ast.Expression.Error)
  }

  def LetExp: Rule1[Ast.Expression.Let] = rule {
    "let" ~ WhiteSpace ~ Ident ~ optWhiteSpace ~ "=" ~ optWhiteSpace ~ Expression ~ WhiteSpace ~ "in" ~ WhiteSpace ~ Expression ~> Ast.Expression.Let
  }

  def IfThenElseExp: Rule1[Ast.Expression.IfThenElse] = rule {
    "if" ~ optWhiteSpace ~ "(" ~ Expression ~ ")" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "else" ~ WhiteSpace ~ Expression ~> Ast.Expression.IfThenElse
  }

  def MatchExp: Rule1[Ast.Expression.Match] = rule {
    "match" ~ WhiteSpace ~ Expression ~ WhiteSpace ~ "with" ~ optWhiteSpace ~ "{" ~ WhiteSpace ~ oneOrMore(MatchRule) ~ "}" ~> Ast.Expression.Match
  }

  def MatchRule: Rule1[(Ast.Pattern, Ast.Expression)] = rule {
    "case" ~ WhiteSpace ~ Pattern ~ WhiteSpace ~ "=>" ~ WhiteSpace ~ Expression ~ ";" ~ optWhiteSpace ~> ((p: Ast.Pattern, e: Ast.Expression) => (p, e))
  }

  def CallExp: Rule1[Ast.Expression.AmbiguousCall] = rule {
    Name ~ "(" ~ zeroOrMore(Expression).separatedBy("," ~ optWhiteSpace) ~ ")" ~> Ast.Expression.AmbiguousCall
  }

  def TupleExp: Rule1[Ast.Expression.Tuple] = rule {
    "(" ~ oneOrMore(Expression).separatedBy("," ~ optWhiteSpace) ~ ")" ~> Ast.Expression.Tuple
  }

  def RecordExp: Rule1[Ast.Expression.Record] = rule {
    "record" ~ WhiteSpace ~ "{" ~ optWhiteSpace ~ zeroOrMore(RecordKeyValue).separatedBy("," ~ optWhiteSpace) ~ optWhiteSpace ~ "}" ~> Ast.Expression.Record
  }

  private def RecordKeyValue: Rule1[(String, Ast.Expression)] = rule {
    Ident ~ optWhiteSpace ~ "=" ~ optWhiteSpace ~ Expression ~> ((k: String, v: Ast.Expression) => (k, v))
  }

  def VariableExp: Rule1[Ast.Expression.AmbiguousName] = rule {
    Name ~> Ast.Expression.AmbiguousName
  }

  def LambdaExp: Rule1[Ast.Expression.Lambda] = rule {
    "fn" ~ optWhiteSpace ~ "(" ~ ArgumentList ~ "):" ~ optWhiteSpace ~ Type ~ optWhiteSpace ~ "=" ~ optWhiteSpace ~ Expression ~> Ast.Expression.Lambda
  }

  /** *************************************************************************/
  /** Patterns                                                              ***/
  /** *************************************************************************/
  def Pattern: Rule1[Ast.Pattern] = rule {
    WildcardPattern | LiteralPattern | AmbigiousPattern | TuplePattern
  }

  def WildcardPattern: Rule1[Ast.Pattern] = rule {
    str("_") ~> (() => Ast.Pattern.Wildcard)
  }

  def LiteralPattern: Rule1[Ast.Pattern] = rule {
    Literal ~> Ast.Pattern.Lit
  }

  def AmbigiousPattern: Rule1[Ast.Pattern.Ambiguous] = rule {
    Name ~ optional(WhiteSpace ~ Pattern) ~> Ast.Pattern.Ambiguous
  }

  def TuplePattern: Rule1[Ast.Pattern.Tuple] = rule {
    "(" ~ oneOrMore(Pattern).separatedBy("," ~ optWhiteSpace) ~ ")" ~> Ast.Pattern.Tuple
  }

  /** *************************************************************************/
  /** Types                                                                 ***/
  /** *************************************************************************/
  def Type: Rule1[Ast.Type] = rule {
    FunctionType
  }

  // NB: Associates to the right, but parsed as left-associative.
  def FunctionType: Rule1[Ast.Type] = rule {
    MapArrowType ~ zeroOrMore(optWhiteSpace ~ "=>" ~ optWhiteSpace ~ MapArrowType ~> Ast.Type.Function)
  }

  // NB: Associates to the right, but parsed as left-associative.
  def MapArrowType: Rule1[Ast.Type] = rule {
    SimpleType ~ zeroOrMore(optWhiteSpace ~ "->" ~ optWhiteSpace ~ SimpleType ~> Ast.Type.Map)
  }

  def SimpleType: Rule1[Ast.Type] = rule {
    TupleType | ListType | SetType | MapType | AmbiguousNameType
  }

  def AmbiguousNameType: Rule1[Ast.Type.AmbiguousName] = rule {
    Name ~> Ast.Type.AmbiguousName
  }

  def TupleType: Rule1[Ast.Type.Tuple] = rule {
    "(" ~ oneOrMore(Type).separatedBy("," ~ optWhiteSpace) ~ ")" ~> Ast.Type.Tuple
  }

  def ListType: Rule1[Ast.Type.List] = rule {
    "List" ~ "[" ~ Type ~ "]" ~> Ast.Type.List
  }

  def SetType: Rule1[Ast.Type.Set] = rule {
    "Set" ~ "[" ~ Type ~ "]" ~> Ast.Type.Set
  }

  def MapType: Rule1[Ast.Type.Map] = rule {
    "Map" ~ "[" ~ Type ~ "," ~ optWhiteSpace ~ Type ~ "]" ~> Ast.Type.Map
  }

  /** *************************************************************************/
  /** Helpers                                                               ***/
  /** *************************************************************************/
  def ArgumentList: Rule1[Seq[(String, Ast.Type)]] = rule {
    zeroOrMore(Argument).separatedBy("," ~ optWhiteSpace)
  }

  def Argument: Rule1[(String, Ast.Type)] = rule {
    Ident ~ ":" ~ optWhiteSpace ~ Type ~> ((name: String, typ: Ast.Type) => (name, typ))
  }

  def Annotation: Rule1[String] = rule {
    "@" ~ Ident ~ WhiteSpace
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

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers                                                             //
  /////////////////////////////////////////////////////////////////////////////
  def Ident2: Rule1[Ast.Ident] = rule {
    SourceLocation ~ capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum | "_") ~ zeroOrMore("'")) ~>
      ((location: lang.SourceLocation, name: String) => Ast.Ident(name, location))
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


  /** *************************************************************************/
  /** Literals                                                              ***/
  /** *************************************************************************/
  def Literal: Rule1[Ast.Literal] = rule {
    UnitLiteral | BoolLiteral | IntLiteral | StrLiteral
  }

  def UnitLiteral: Rule1[Ast.Literal.Unit.type] = rule {
    str("()") ~> (() => Ast.Literal.Unit)
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
