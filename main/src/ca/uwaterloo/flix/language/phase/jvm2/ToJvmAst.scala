/*
 * Copyright 2025 Cade Lueker
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{JvmAst, MonoType, ReducedAst}
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugJvmAst

import java.lang.constant.ConstantDescs
import java.lang.constant.ClassDesc

object ToJvm {

  def run(root: ReducedAst.Root)(implicit flix: Flix): JvmAst.Root = flix.phase("ToJvm") {
    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    val anonClasses = root.anonClasses.map(visitAnonClass)
    val jvmRootTypes = root.types.map(toJvmType)
    JvmAst.Root(defs, enums, structs, effects, jvmRootTypes, anonClasses, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  /** converting [[tpe]] to unboxed Java type, lower case is primitive */
  private def toJvmType(tpe: MonoType): ClassDesc = tpe match {
    case MonoType.Void => ConstantDescs.CD_Object
    case MonoType.AnyType => ConstantDescs.CD_Object
    case MonoType.Unit => ClassDesc.of("dev.flix.runtime.Unit") // flix/language/phase/jvm/BackendObjType
    case MonoType.Bool => ConstantDescs.CD_boolean
    case MonoType.Char => ConstantDescs.CD_char
    case MonoType.Float32 => ConstantDescs.CD_float
    case MonoType.Float64 => ConstantDescs.CD_double // primitive type
    case MonoType.BigDecimal => ClassDesc.of("java.math.BigDecimal")
    case MonoType.Int8 => ConstantDescs.CD_byte
    case MonoType.Int16 => ConstantDescs.CD_short
    case MonoType.Int32 => ConstantDescs.CD_int
    case MonoType.Int64 => ConstantDescs.CD_long
    case MonoType.BigInt => ClassDesc.of("java.math.BigInteger")
    case MonoType.String => ConstantDescs.CD_String
    case MonoType.Regex => ClassDesc.of("java.util.regex.Pattern")
    case MonoType.Region => ConstantDescs.CD_Object // not sure about this one
    case MonoType.Null => ConstantDescs.CD_Object // there is 'ConstantDescs.Null' but it isn't a ClassDesc
    case MonoType.Array(t) => toJvmType(t).arrayType()
    case MonoType.Lazy(t) =>
      val _jvmType = toJvmType(t)
      ???
    case MonoType.Tuple(tpes) =>
      val _len = tpes.size
      val _typeString = tpes.map(toJvmType).foldLeft("")({ (acc, t) => acc + t.toString + "," }).dropRight(1)
      ???

    case MonoType.Enum(sym, targs) =>
      val _typeString = targs.map(toJvmType).foldLeft("")({ (acc, t) => acc + t.toString + ","}).dropRight(1)
      ???
    case MonoType.Struct(sym, targs) =>
      val _typeString = targs.map(toJvmType).foldLeft("")({ (acc, t) => acc + t.toString + ","}).dropRight(1)
      ???
    case MonoType.Arrow(args, result) =>
      val _argTypes = args.map(toJvmType)
      val _resType = toJvmType(result)
      ???
    case MonoType.RecordEmpty => ???
    case MonoType.RecordExtend(label, value, rest) =>
      val _valType = toJvmType(value)
      val _resType = toJvmType(rest)
      ???
    case MonoType.ExtensibleEmpty => ???
    case MonoType.ExtensibleExtend(cons, tpes, rest) =>
      val _jvmTypes = tpes.map(toJvmType)
      val _resType = toJvmType(rest)
      ???
    case MonoType.Native(clazz) => ???
  }

  private def visitDef(defn: ReducedAst.Def): JvmAst.Def = {
    val cparams = defn.cparams.map(visitFParam)
    val fparams = defn.fparams.map(visitFParam)
    val lparams = defn.lparams.map(visitLocalParam)
    val expr = visitExpr(defn.expr)
    val unboxedType = visitUnboxedType(defn.unboxedType)
    JvmAst.Def(
      defn.ann,
      defn.mod,
      defn.sym,
      cparams,
      fparams,
      lparams,
      defn.pcPoints,
      expr,
      toJvmType(defn.tpe),
      unboxedType,
      defn.loc,
    )
  }

  private def visitFParam(fp: ReducedAst.FormalParam): JvmAst.FormalParam = {
    JvmAst.FormalParam(
      fp.sym,
      fp.mod,
      toJvmType(fp.tpe),
      fp.loc,
    )
  }

  private def visitLocalParam(lp: ReducedAst.LocalParam): JvmAst.LocalParam = {
    JvmAst.LocalParam(
      lp.sym,
      toJvmType(lp.tpe)
    )
  }

  private def visitExpr(expr: ReducedAst.Expr): JvmAst.Expr =
    expr match {
      case ReducedAst.Expr.Cst(cst, tpe, loc) =>
        JvmAst.Expr.Cst(
          cst,
          toJvmType(tpe),
          loc,
        )

      case ReducedAst.Expr.Var(sym, tpe, loc) =>
        JvmAst.Expr.Var(
          sym,
          toJvmType(tpe),
          loc,
        )

      case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.ApplyAtomic(op, es, jvmTpe, purity, loc)

      case ReducedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.ApplyClo(e1, e2, ct, jvmTpe, purity, loc)

      case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.ApplyDef(sym, es, ct, jvmTpe, purity, loc)

      case ReducedAst.Expr.ApplySelfTail(sym, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.ApplySelfTail(sym, es, jvmTpe, purity, loc)

      case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val e3 = visitExpr(exp3)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.IfThenElse(e1, e2, e3, jvmTpe, purity, loc)

      case ReducedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val bs = branches map {
          case (label, body) => label -> visitExpr(body)
        }
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.Branch(e, bs, jvmTpe, purity, loc)

      case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        JvmAst.Expr.JumpTo(
          sym,
          toJvmType(tpe),
          purity,
          loc,
        )

      case ReducedAst.Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.Let(sym, e1, e2, jvmTpe, purity, loc)

      case ReducedAst.Expr.Stmt(exp1, exp2, tpe, purity, loc) =>
        val e1 = visitExpr(exp1)
        val e2 = visitExpr(exp2)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.Stmt(e1, e2, jvmTpe, purity, loc)

      case ReducedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.Scope(sym, e, jvmTpe, purity, loc)

      case ReducedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val rs = rules map {
          case ReducedAst.CatchRule(sym, clazz, body) =>
            val b = visitExpr(body)
            JvmAst.CatchRule(sym, clazz, b)
        }
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.TryCatch(e, rs, jvmTpe, purity, loc)

      case ReducedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
        val e = visitExpr(exp)
        val rs = rules.map {
          case ReducedAst.HandlerRule(op, fparams, body) =>
            val b = visitExpr(body)
            JvmAst.HandlerRule(op, fparams.map(visitFParam), b)
        }
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.RunWith(e, effUse, rs, ct, jvmTpe, purity, loc)

      case ReducedAst.Expr.Do(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExpr)
        val jvmTpe = toJvmType(tpe)
        JvmAst.Expr.Do(op, es, jvmTpe, purity, loc)

      case ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
        val specs = methods.map {
          case ReducedAst.JvmMethod(ident, fparams, clo, retTpe, methPurity, methLoc) =>
            val c = visitExpr(clo)
            val jvmRetTpe = toJvmType(retTpe)
            JvmAst.JvmMethod(ident, fparams.map(visitFParam), c, jvmRetTpe, methPurity, methLoc)
        }
        val jvmTpe = toJvmType(tpe)

        JvmAst.Expr.NewObject(name, clazz, jvmTpe, purity, specs, loc)

    }

  private def visitCase(cse: ReducedAst.Case): JvmAst.Case = {
    JvmAst.Case(cse.sym, cse.tpes, cse.loc)
  }

  private def visitEnum(enm: ReducedAst.Enum): JvmAst.Enum = {
    JvmAst.Enum(
      enm.ann,
      enm.mod,
      enm.sym,
      enm.tparams.map(visitTParam),
      enm.cases.map({ case (sym, cse) => sym -> visitCase(cse) }),
      enm.loc,
    )
  }

  private def visitUnboxedType(unboxedType: ReducedAst.UnboxedType): JvmAst.UnboxedType = {
    JvmAst.UnboxedType(toJvmType(unboxedType.tpe))
  }

  private def visitStruct(struct: ReducedAst.Struct): JvmAst.Struct = {
    val tparams = struct.tparams.map(visitTParam)
    val fields = struct.fields.map(visitField)
    JvmAst.Struct(struct.ann, struct.mod, struct.sym, tparams, fields, struct.loc)
  }

  private def visitTParam(value: ReducedAst.TypeParam): JvmAst.TypeParam = {
    JvmAst.TypeParam(value.name, value.sym, value.loc)
  }

  private def visitField(field: ReducedAst.StructField): JvmAst.StructField = {
    JvmAst.StructField(field.sym, field.tpe, field.loc)
  }

  private def visitOp(op: ReducedAst.Op): JvmAst.Op = {
    JvmAst.Op(op.sym, op.ann, op.mod, op.fparams.map(visitFParam), toJvmType(op.tpe), op.purity, op.loc)
  }

  private def visitEffect(effect: ReducedAst.Effect): JvmAst.Effect = {
    JvmAst.Effect(effect.ann, effect.mod, effect.sym, effect.ops.map(visitOp), effect.loc)
  }

  private def visitJvmMethod(jvmMethod: ReducedAst.JvmMethod): JvmAst.JvmMethod = {
    JvmAst.JvmMethod(
      jvmMethod.ident,
      jvmMethod.fparams.map(visitFParam),
      visitExpr(jvmMethod.exp),
      toJvmType(jvmMethod.tpe),
      jvmMethod.purity,
      jvmMethod.loc,
    )
  }
  private def visitAnonClass(anonClass: ReducedAst.AnonClass): JvmAst.AnonClass = {
    JvmAst.AnonClass(
      anonClass.name,
      anonClass.clazz,
      toJvmType(anonClass.tpe),
      anonClass.methods.map(visitJvmMethod),
      anonClass.loc
    )
  }
}
