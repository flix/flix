package ca.uwaterloo.flix.language.backend.ir

import ca.uwaterloo.flix.language.ast.{Name, BinaryOperator, SourceLocation, UnaryOperator}
import ca.uwaterloo.flix.runtime.Value

sealed trait CodeGenIR

object CodeGenIR {

  case class ValuePool(strings: StringPool) {
    def valueOf(int: Int, tpe: CodeGenIR.Type): Value = ???
  }

  case class StringPool(xs: Array[String])

  sealed trait Definition

  object Definition {

    case class Function(name: Name.Resolved, args: List[LocalVar], body: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Definition

  }

  sealed trait Expression

  object Expression {

    case class Int(literal: Int, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Var(localVar: CodeGenIR.LocalVar, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Ref(name: Name.Resolved, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Apply(name: Name.Resolved, args: List[CodeGenIR.Expression], tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Let(localVar: CodeGenIR.LocalVar, exp1: CodeGenIR.Expression, exp2: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Unary(op: UnaryOperator, exp: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Binary(op: BinaryOperator, exp1: CodeGenIR.Expression, exp2: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class IfThenElse(exp1: CodeGenIR.Expression, exp2: CodeGenIR.Expression, exp3: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    // TODO: ElementAt( (1,2), 0) = 1
    case class ElementAt(exp1: CodeGenIR.Expression, exp2: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Tag(tag: TagName, exp: CodeGenIR.Expression, tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class DerefTag() extends CodeGenIR.Expression

    case class Tuple(elms: List[CodeGenIR.Expression], tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

    case class Set(elms: List[CodeGenIR.Expression], tpe: CodeGenIR.Type.Set, loc: SourceLocation) extends CodeGenIR.Expression

    case class Error(tpe: CodeGenIR.Type, loc: SourceLocation) extends CodeGenIR.Expression

  }

  // TODO: String

  sealed trait Type

  object Type {

    case object Int extends CodeGenIR.Type

    case class Set() extends CodeGenIR.Type

    // TODO: Function

  }

  case class LocalVar(offset: Int) extends CodeGenIR

  case class TagName(x: Int) extends CodeGenIR

}
