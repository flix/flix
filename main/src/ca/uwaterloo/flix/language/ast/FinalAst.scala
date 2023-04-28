/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.Source

import java.lang.reflect.{Constructor, Field, Method}

object FinalAst {

  case class Root(defs: Map[Symbol.DefnSym, FinalAst.Def],
                  enums: Map[Symbol.EnumSym, FinalAst.Enum],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, formals: List[FinalAst.FormalParam], exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) {
    var method: Method = _
  }

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, cases: Map[Symbol.CaseSym, FinalAst.Case], tpeDeprecated: MonoType, loc: SourceLocation)

  sealed trait Expression {
    def tpe: MonoType

    def loc: SourceLocation
  }

  object Expression {

    case class Cst(cst: Ast.Constant, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Var(sym: Symbol.VarSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Closure(sym: Symbol.DefnSym, closureArgs: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyAtomic(op: AtomicOp, exps: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyClo(exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyCloTail(exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ApplySelfTail(sym: Symbol.DefnSym, formals: List[FinalAst.FormalParam], actuals: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class IfThenElse(exp1: FinalAst.Expression, exp2: FinalAst.Expression, exp3: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Branch(exp: FinalAst.Expression, branches: Map[Symbol.LabelSym, FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class LetRec(varSym: Symbol.VarSym, index: Int, defSym: Symbol.DefnSym, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Region(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Scope(sym: Symbol.VarSym, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ScopeExit(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Is(sym: Symbol.CaseSym, exp: FinalAst.Expression, loc: SourceLocation) extends FinalAst.Expression {
      final val tpe: MonoType = MonoType.Bool
    }

    case class Tag(sym: Symbol.CaseSym, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Untag(sym: Symbol.CaseSym, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Index(base: FinalAst.Expression, offset: scala.Int, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Tuple(elms: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordEmpty(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordSelect(exp: FinalAst.Expression, field: Name.Field, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordExtend(field: Name.Field, value: FinalAst.Expression, rest: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class RecordRestrict(field: Name.Field, rest: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLit(elms: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayNew(elm: FinalAst.Expression, len: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLoad(base: FinalAst.Expression, index: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayStore(base: FinalAst.Expression, index: FinalAst.Expression, elm: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class ArrayLength(base: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Ref(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Deref(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Assign(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class InstanceOf(exp: FinalAst.Expression, clazz: java.lang.Class[_], loc: SourceLocation) extends FinalAst.Expression {
      final val tpe: MonoType = MonoType.Bool
    }

    case class Cast(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class TryCatch(exp: FinalAst.Expression, rules: List[FinalAst.CatchRule], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class InvokeMethod(method: Method, exp: FinalAst.Expression, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[FinalAst.Expression], tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class GetField(field: Field, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class PutField(field: Field, exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class GetStaticField(field: Field, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class PutStaticField(field: Field, exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: MonoType, methods: List[FinalAst.JvmMethod], loc: SourceLocation) extends FinalAst.Expression

    case class Spawn(exp1: FinalAst.Expression, exp2: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Lazy(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class Force(exp: FinalAst.Expression, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class HoleError(sym: Symbol.HoleSym, tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

    case class MatchError(tpe: MonoType, loc: SourceLocation) extends FinalAst.Expression

  }

  case class Case(sym: Symbol.CaseSym, tpeDeprecated: MonoType, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FinalAst.FormalParam], clo: FinalAst.Expression, retTpe: MonoType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: FinalAst.Expression)

  case class FormalParam(sym: Symbol.VarSym, tpe: MonoType)

}
