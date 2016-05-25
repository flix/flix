package ca.uwaterloo.flix.language.ast

import scala.collection.immutable.List

trait NamedAst

object NamedAst {

  case class Program(enums: Map[Name.NName, Map[String, NamedAst.Declaration.Enum]],
                     lattices: Map[Type, NamedAst.Declaration.BoundedLattice],
                     indexes: Map[Symbol.TableSym, NamedAst.Declaration.Index],
                     tables: Map[Name.NName, Map[String, NamedAst.Table]],
                     facts: List[NamedAst.Declaration.Fact],
                     rules: List[NamedAst.Declaration.Rule],
                     hooks: Map[Symbol.Resolved, Ast.Hook],
                     time: Time) extends NamedAst

  sealed trait Declaration extends NamedAst {
    def loc: SourceLocation
  }

  object Declaration {

    case class Definition(ann: Ast.Annotations, ident: Name.Ident, params: List[Ast.FormalParam], exp: NamedAst.Expression, tpe: Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Signature(ident: Name.Ident, params: List[Ast.FormalParam], tpe: Type, loc: SourceLocation) extends NamedAst.Declaration

    case class External(ident: Name.Ident, params: List[Ast.FormalParam], tpe: Type, loc: SourceLocation) extends NamedAst.Declaration

    case class Law(ident: Name.Ident, tparams: List[ParsedAst.ContextBound], params: List[Ast.FormalParam], tpe: Type, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Declaration

    case class Enum(ident: Name.Ident, cases: Map[String, NamedAst.Case], loc: SourceLocation) extends NamedAst.Declaration

    case class Class(ident: Name.Ident, tparams: List[Type], /* bounds: List[ContextBound],*/ decls: List[NamedAst.Declaration], loc: SourceLocation) extends NamedAst.Declaration

    case class Impl(ident: Name.Ident, tparams: List[Type], /*bounds: List[ContextBound],*/ decls: List[NamedAst.Declaration], loc: SourceLocation) extends NamedAst.Declaration

    case class Fact(head: NamedAst.Predicate.Head, loc: SourceLocation) extends NamedAst.Declaration

    case class Rule(head: NamedAst.Predicate.Head, body: List[NamedAst.Predicate.Body], loc: SourceLocation) extends NamedAst.Declaration

    case class Index(ident: Name.Ident, indexes: List[List[Name.Ident]], loc: SourceLocation) extends NamedAst.Declaration

    @deprecated("Will be replaced by type classes", "0.1.0")
    case class BoundedLattice(tpe: Type, bot: NamedAst.Expression, top: NamedAst.Expression, leq: NamedAst.Expression, lub: NamedAst.Expression, glb: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Declaration

  }

  sealed trait Table extends NamedAst.Declaration {
    def sym: Symbol.TableSym

    def loc: SourceLocation
  }

  object Table {

    case class Relation(sym: Symbol.TableSym, attr: List[Ast.Attribute], loc: SourceLocation) extends NamedAst.Table

    case class Lattice(sym: Symbol.TableSym, keys: List[Ast.Attribute], value: Ast.Attribute, loc: SourceLocation) extends NamedAst.Table

  }

  sealed trait Expression extends NamedAst {
    def id: Int

    def loc: SourceLocation
  }

  object Expression {

    case class Wild(id: Int, loc: SourceLocation) extends NamedAst.Expression

    case class Var(id: Int, sym: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Expression

    case class Ref(id: Int, ref: Name.QName, loc: SourceLocation) extends NamedAst.Expression

    case class Unit(id: Int, loc: SourceLocation) extends NamedAst.Expression

    case class True(id: Int, loc: SourceLocation) extends NamedAst.Expression

    case class False(id: Int, loc: SourceLocation) extends NamedAst.Expression

    case class Char(id: Int, lit: scala.Char, loc: SourceLocation) extends NamedAst.Expression

    case class Float32(id: Int, lit: scala.Float, loc: SourceLocation) extends NamedAst.Expression

    case class Float64(id: Int, lit: scala.Double, loc: SourceLocation) extends NamedAst.Expression

    case class Int8(id: Int, lit: scala.Byte, loc: SourceLocation) extends NamedAst.Expression

    case class Int16(id: Int, lit: scala.Short, loc: SourceLocation) extends NamedAst.Expression

    case class Int32(id: Int, lit: scala.Int, loc: SourceLocation) extends NamedAst.Expression

    case class Int64(id: Int, lit: scala.Long, loc: SourceLocation) extends NamedAst.Expression

    case class BigInt(id: Int, lit: java.math.BigInteger, loc: SourceLocation) extends NamedAst.Expression

    case class Str(id: Int, lit: java.lang.String, loc: SourceLocation) extends NamedAst.Expression

    case class Apply(id: Int, lambda: NamedAst.Expression, args: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class Lambda(id: Int, params: List[Symbol.VarSym], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Unary(id: Int, op: UnaryOperator, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Binary(id: Int, op: BinaryOperator, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class IfThenElse(id: Int, exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Let(id: Int, sym: Symbol.VarSym, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Match(id: Int, exp: NamedAst.Expression, rules: List[(NamedAst.Pattern, NamedAst.Expression)], loc: SourceLocation) extends NamedAst.Expression

    case class Switch(id: Int, rules: List[(NamedAst.Expression, NamedAst.Expression)], loc: SourceLocation) extends NamedAst.Expression

    case class Tag(id: Int, enum: Name.QName, tag: Name.Ident, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Tuple(id: Int, elms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class FNone(id: Int, loc: SourceLocation) extends NamedAst.Expression

    case class FSome(id: Int, exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FNil(id: Int, loc: SourceLocation) extends NamedAst.Expression

    case class FList(id: Int, hd: NamedAst.Expression, tl: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class FVec(id: Int, elms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class FSet(id: Int, elms: List[NamedAst.Expression], loc: SourceLocation) extends NamedAst.Expression

    case class FMap(id: Int, elms: List[(NamedAst.Expression, NamedAst.Expression)], loc: SourceLocation) extends NamedAst.Expression

    case class GetIndex(id: Int, exp1: NamedAst.Expression, exp2: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class PutIndex(id: Int, exp1: NamedAst.Expression, exp2: NamedAst.Expression, exp3: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Existential(id: Int, params: List[Ast.FormalParam], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Universal(id: Int, params: List[Ast.FormalParam], exp: NamedAst.Expression, loc: SourceLocation) extends NamedAst.Expression

    case class Ascribe(id: Int, exp: NamedAst.Expression, tpe: Type, loc: SourceLocation) extends NamedAst.Expression

    case class UserError(id: Int, loc: SourceLocation) extends NamedAst.Expression

  }

  sealed trait Pattern extends NamedAst {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends NamedAst.Pattern

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends NamedAst.Pattern

    case class Unit(loc: SourceLocation) extends NamedAst.Pattern

    case class True(loc: SourceLocation) extends NamedAst.Pattern

    case class False(loc: SourceLocation) extends NamedAst.Pattern

    case class Char(lit: scala.Char, loc: SourceLocation) extends NamedAst.Pattern

    case class Float32(lit: scala.Float, loc: SourceLocation) extends NamedAst.Pattern

    case class Float64(lit: scala.Double, loc: SourceLocation) extends NamedAst.Pattern

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends NamedAst.Pattern

    case class Int16(lit: scala.Short, loc: SourceLocation) extends NamedAst.Pattern

    case class Int32(lit: scala.Int, loc: SourceLocation) extends NamedAst.Pattern

    case class Int64(lit: scala.Long, loc: SourceLocation) extends NamedAst.Pattern

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends NamedAst.Pattern

    case class Str(lit: java.lang.String, loc: SourceLocation) extends NamedAst.Pattern

    case class Tag(enum: Name.QName, tag: Name.Ident, pat: NamedAst.Pattern, loc: SourceLocation) extends NamedAst.Pattern

    case class Tuple(elms: scala.List[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Pattern

    case class FNone(loc: SourceLocation) extends NamedAst.Pattern

    case class FSome(pat: NamedAst.Pattern, loc: SourceLocation) extends NamedAst.Pattern

    case class FNil(loc: SourceLocation) extends NamedAst.Pattern

    case class FList(hd: NamedAst.Pattern, tl: NamedAst.Pattern, loc: SourceLocation) extends NamedAst.Pattern

    case class FVec(elms: List[NamedAst.Pattern], rest: Option[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Pattern

    case class FSet(elms: List[NamedAst.Pattern], rest: Option[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Pattern

    case class FMap(elms: List[(NamedAst.Pattern, NamedAst.Pattern)], rest: Option[NamedAst.Pattern], loc: SourceLocation) extends NamedAst.Pattern

  }

  // TODO: Cleanup this stuff:

  sealed trait Predicate extends NamedAst

  object Predicate {

    sealed trait Head extends NamedAst.Predicate

    object Head {

      case class Table(name: Name.QName, terms: List[WeededAst.Term.Head], loc: SourceLocation) extends NamedAst.Predicate.Head

    }

    sealed trait Body extends NamedAst.Predicate

    object Body {

      case class Ambiguous(name: Name.QName, terms: List[WeededAst.Term.Body], loc: SourceLocation) extends NamedAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, loc: SourceLocation) extends NamedAst.Predicate.Body

      case class Loop(ident: Name.Ident, term: WeededAst.Term.Head, loc: SourceLocation) extends NamedAst.Predicate.Body

    }

  }

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: Type)

}
