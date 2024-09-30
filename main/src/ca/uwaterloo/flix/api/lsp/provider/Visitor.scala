/*
 * Copyright 2024 Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.Entity
import ca.uwaterloo.flix.language.ast.TypedAst.{
  Root, 
  Def, 
  Expr, 
  Effect, 
  Enum,
  Constraint, 
  Pattern, 
  StructField,
  Case,
  Instance,
  RestrictableEnum,
  RestrictableCase,
  Sig,
  Struct,
  Trait,
  TypeAlias,
  TypeParam
}
import ca.uwaterloo.flix.language.ast.Ast.UseOrImport
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.api.lsp.Position

/**
  * A collection of visit functions that can apply a set of functions
  * to an TypedAst node and all of it's children recursively.
  */
object Visitor {
  /**
    * Applies a `seenRoot` to the root AST node and recursively visits
    * all children, applying the corresponding `seenX` functions.
    *
    * @param root    The AST root node.
    * @param seenX   The `seen` function that is to be applied to X type AST nodes.
    * @param accept  A predicate determining whether to visit a child node.
    */
  def visitRoot(root: Root, 
                seenDef: Def => Unit,
                seenEff: Effect => Unit,
                seenEnum: Enum => Unit,
                seenExpr: Expr => Unit,
                seenInstance: Instance => Unit,
                seenResEnum: RestrictableEnum => Unit,
                seenRoot: Root => Unit,
                seenSig: Sig => Unit,
                seenStruct: Struct => Unit,
                seenTrait: Trait => Unit,
                seenTypeAlias: TypeAlias => Unit,
                accept: SourceLocation => Boolean): Unit = {

    root.defs.foreach{ case (_, defn) => {
      val insideDefn = accept(defn.spec.loc) || accept(defn.exp.loc) || accept(defn.sym.loc)
      if (insideDefn) {
        visitDef(defn, seenDef, seenExpr, accept)
      }
    }}

    root.effects.foreach{ case (_, eff) => {
      val insideEff = accept(eff.loc) || eff.ann.annotations.exists(ann => accept(ann.loc)) || accept(eff.sym.loc)
      if (insideEff) {
        visitEffect(eff, seenEff, accept)
      }
    }}

    // root.entryPoint.map{ case v => visitEntryPoint(visit, accept)(v) }
    root.enums.foreach{ case (_, e) => {
      if (accept(e.loc)) { visitEnum(e, seenEnum, accept) } }
    }

    root.instances.foreach{ case (_, l) => {
      l.foreach(ins => if (accept(ins.loc)) { visitInstance(ins, seenInstance, accept) }) }
    }

    // root.modules.map { case (_, v) => visitModule(v, ???, ???) }

    root.restrictableEnums.foreach{ case (_, e) => {
      if (accept(e.loc)) { visitResEnum(e, seenResEnum, accept) } } // experimental, maybe should be removed?
    }

    root.sigs.foreach{ case (_, sig) => {
      val insideSig = accept(sig.spec.loc) || sig.exp.exists(e => accept(e.loc)) || accept(sig.sym.loc)
      if (insideSig) {
        visitSig(sig, seenSig, accept)
      }
    }}

    root.structs.foreach{ case (_, struct) => {
      val insideStruct = accept(struct.loc) || struct.fields.exists{ case (s, c) => accept(s.loc) || accept(c.loc) }
      if (insideStruct) {
        visitStruct(struct, seenStruct, accept)
      }
    }}

    // root.traitEnv.map{ case (_, v) => visitTraitEnv(???, ???)(v) };

    root.traits.foreach{ case (_, t) => {
      if (accept(t.loc)) { visitTrait(t, seenTrait, accept) }
    }}

    root.typeAliases.foreach{ case (_, alias) => {
      if (accept(alias.loc)) { visitTypeAlias(alias, seenTypeAlias, accept) }
    }}

    // root.uses
  }

  def inside(uri: String, pos: Position)(loc: SourceLocation): Boolean = {
    val (x, y) = pos.toZeroIndexed

    val posLine = x + 1
    val posCol = y + 1

    // sp1 and sp2, by invariant, has the same source, so we can use either
    (uri == loc.sp1.source.name) &&
    (posLine >= loc.beginLine) &&
    (posLine <= loc.endLine) &&
    (!(posLine == loc.beginLine && posCol < loc.beginCol)) &&
    (!(posLine == loc.endLine && posCol >= loc.endCol)) // geq because end column is exclusive
  }

  def inside(loc1: SourceLocation, loc2: SourceLocation): Boolean = {
    loc1.source == loc2.source &&
    (loc1.beginLine >= loc2.beginLine) &&
    (loc1.endLine <= loc2.endLine) &&
    !(loc2.beginLine == loc1.beginLine && loc2.beginCol > loc1.beginCol) &&
    !(loc1.endLine == loc2.endLine && loc1.endCol > loc2.endCol)
  }

  def visitEnum(e: Enum, seen: Enum => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(e)
    // e.cases
    //  .map{case (_, c) => c}
    //  .foreach(c => if (accept(c.loc)) { visitCase(c, ???, accept) })
  }

  def visitCase(c: Case, seen: Case => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(c)
  }

  def visitResEnum(e: RestrictableEnum, seen: RestrictableEnum => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(e)
    // e.cases
    //  .map{case (_, c) => c}
    //  .foreach(c => if (accept(c.loc)) { visitResCase(c, ???, accept) })
  }

  def visitResCase(c: RestrictableCase, seen: RestrictableCase => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(c)
  }

  def visitInstance(ins: Instance, seen: Instance => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(ins)
    // ins.defs.foreach(defn => visitDef(defn, ???, accept))
  }

  def visitSig(sig: Sig, seen: Sig => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(sig)
    // if (accept(sig.spec.loc)) {
    //   ??? // TODO
    // }

    // sig.exp.foreach(exp =>
    //   if (accept(exp.loc)) {
    //     visitExpr(exp, ???, accept)
    //   }
    // )
  }

  def visitStruct(struct: Struct, seen: Struct => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(struct)
    // struct.fields.foreach{case (_, field) => {
    //   if (accept(field.loc) ) { 
    //     visitStructField(field, ???, accept) 
    //   }
    // }}
  }

  def visitStructField(field: StructField, seen: StructField => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(field)
  }

  def visitTrait(t: Trait, seen: Trait => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(t)
    // t.laws.foreach(law => visitDef(law, ???, accept))
    // t.sigs.foreach(sig => visitSig(sig, ???, accept))
  }
  
  def visitEffect(eff: Effect, seen: Effect => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(eff)
    // ???
  }

  def visitDef(defn: Def, seenDef: Def => Unit, seenExpr: Expr => Unit, accept: SourceLocation => Boolean): Unit = {
    seenDef(defn)
    if (accept(defn.spec.loc)) {
      // TODO
    }

    if (accept(defn.exp.loc)) {
      visitExpr(defn.exp, seenExpr, accept)
    }
  }

  def visitTypeAlias(alias: TypeAlias, seen: TypeAlias => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(alias)
    // alias.tparams.map(tp => if (accept(tp.loc)) { visitTypeParam(tp, ???, accept) })
  }

  def visitTypeParam(tparam: TypeParam, seen: TypeParam => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(tparam)
  }

  def visitUse(use: UseOrImport, seen: UseOrImport => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
    // visit(use)
  }

  def visitExpr(expr: Expr, seen: Expr => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO: handle mutually recursive calls to other visit functions

    seen(expr)
    expr match {
      case Expr.Cst(cst, tpe, loc) => ()
      case Expr.Var(sym, tpe, loc) => ()
      case Expr.Def(sym, tpe, loc) => ()
      case Expr.Sig(sym, tpe, loc) => ()
      case Expr.Hole(sym, tpe, eff, loc) => ()

      case Expr.HoleWithExp(exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }
      }

      case Expr.OpenAs(symUse, exp, tpe, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }
      }

      case Expr.Use(sym, alias, exp, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }
      }

      case Expr.Lambda(fparam, exp, tpe, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }
      }

      case Expr.Apply(exp, exps, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }
        exps.foreach(e => {
          if (accept(e.loc)) { visitExpr(e, seen, accept) }
        })
      }

      case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        exps.foreach(e => {
          if (accept(e.loc)) { visitExpr(e, seen, accept) }
        })

      case Expr.Unary(sop, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }
      }

      case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.Region(tpe, loc) => ()

      case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }
        if (accept(exp3.loc)) { visitExpr(exp3, seen, accept) }

      case Expr.Stm(exp1, exp2, _, _, _) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.Discard(exp, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.Match(exp, rules, tpe, eff, loc) =>
        rules.foreach(rule => {
          if (accept(rule.exp.loc)) { visitExpr(rule.exp, seen, accept) }
          rule.guard.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })
          // TODO
          // visitPattern(rule.pat, ???, accept)
        })

        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
        // Does nothing because feature is experimental

      case Expr.Tag(sym, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.RestrictableTag(sym, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.Tuple(exps, tpe, eff, loc) =>
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })

      case Expr.RecordEmpty(tpe, loc) => ()

      case Expr.RecordSelect(exp, label, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })

      case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }
        if (accept(exp3.loc)) { visitExpr(exp3, seen, accept) }

      case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.ArrayLength(exp, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.StructNew(sym, fields, region, tpe, eff, loc) =>
        fields.foreach{ case (_, e) => { 
          if (accept(e.loc)) { visitExpr(e, seen, accept) } 
        }}

        if (accept(region.loc)) { visitExpr(region, seen, accept) }

      case Expr.StructGet(exp, sym, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.VectorLit(exps, tpe, eff, loc) =>
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })

      case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.VectorLength(exp, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.Ascribe(exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.InstanceOf(exp, clazz, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.UncheckedCast(exp, decalredType, declaredEff, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.Without(exp, effUse, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}
        rules.foreach(rule => if (accept(rule.exp.loc)) { visitExpr(rule.exp, seen, accept)} )

      case Expr.Throw(exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}
        rules.foreach(rule => if (accept(rule.exp.loc)) { visitExpr(rule.exp, seen, accept)} )

      case Expr.Do(op, exps, tpe, eff, loc) =>
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })

      case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })

      case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })

      case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })

      case Expr.GetField(field, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.GetStaticField(field, tpe, eff, loc) => ()

      case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept) }

      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ()

      case Expr.NewChannel(exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.GetChannel(exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.SelectChannel(rules, default, tpe, eff, loc) =>
        rules.foreach(rule => {
          if (accept(rule.chan.loc)) { visitExpr(rule.chan, seen, accept) }
          if (accept(rule.exp.loc)) { visitExpr(rule.exp, seen, accept)}
        })

        default.foreach(e => if (accept(e.loc)) { visitExpr(e, seen, accept) })

      case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.ParYield(frags, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}
        frags.foreach(frag => {
          if (accept(frag.exp.loc)) { visitExpr(frag.exp, seen, accept)}
          // TODO: visit frag.pat (pattern)
        })

      case Expr.Lazy(exp, tpe, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.Force(exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.FixpointConstraintSet(cs, tpe, loc) =>
        // TODO
        // cs.foreach(con => visitConstraint(con, ???, accept))

      case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, seen, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, seen, accept) }

      case Expr.FixpointSolve(exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.FixpointFilter(pred, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.FixpointInject(exp, pred, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
        if (accept(exp.loc)) { visitExpr(exp, seen, accept)}

      case Expr.Error(m, tpe, eff) => ()
    }
  }

  def visitConstraint(con: Constraint, visit: Constraint => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }

  def visitPattern(pat: Pattern, visit: Pattern => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO
  }
}
