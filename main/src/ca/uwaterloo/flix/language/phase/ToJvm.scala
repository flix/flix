/*
 * Copyright 20XX Name
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
import ca.uwaterloo.flix.language.ast.{JvmAst, ReducedAst}
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugJvmAst

object ToJvm {

  def run(root: ReducedAst.Root)(implicit flix: Flix): JvmAst.Root = flix.phase("ToJvm") {
    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    val anonClasses = root.anonClasses.map(visitAnonClass)
    JvmAst.Root(defs, enums, structs, effects, root.types, anonClasses, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  private def visitDef(defn: ReducedAst.Def): JvmAst.Def = {
    val cparams = defn.cparams.map(visitFParam)
    val fparams = defn.fparams.map(visitFParam)
    val lparams = defn.lparams.map(visitLocalParam)
    val expr = visitExpr(defn.expr)
    val unboxedType = visitUnboxedType(defn.unboxedType)
    JvmAst.Def(defn.ann, defn.mod, defn.sym, cparams, fparams, lparams, defn.pcPoints, expr, defn.tpe, unboxedType, defn.loc)
  }

  private def visitFParam(fp: ReducedAst.FormalParam): JvmAst.FormalParam = {
    JvmAst.FormalParam(fp.sym, fp.mod, fp.tpe, fp.loc)
  }

  private def visitLocalParam(lp: ReducedAst.LocalParam): JvmAst.LocalParam = {
    JvmAst.LocalParam(lp.sym, lp.tpe)
  }

  private def visitExpr(expr: ReducedAst.Expr): JvmAst.Expr = {
    ???
  }

  private def visitEnum(enm: ReducedAst.Enum): JvmAst.Enum = {
    ???
  }

  private def visitUnboxedType(unboxedType: ReducedAst.UnboxedType): JvmAst.UnboxedType = {
    JvmAst.UnboxedType(unboxedType.tpe)
  }

  private def visitStruct(struct: ReducedAst.Struct): JvmAst.Struct = {
    val tparams = struct.tparams.map(visitTParam)
    val fields = struct.fields.map(visitField)
    JvmAst.Struct(struct.ann, struct.mod, struct.sym, tparams, fields, struct.loc)
  }

  private def visitTParam(value: ReducedAst.TypeParam): JvmAst.TypeParam = {
    ???
  }

  private def visitField(field: ReducedAst.StructField): JvmAst.StructField = {
    ???
  }

  private def visitEffect(effect: ReducedAst.Effect): JvmAst.Effect = {
    ???
  }

  private def visitAnonClass(anonClass: ReducedAst.AnonClass): JvmAst.AnonClass = {
    ???
  }
}
