package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.Entity
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.TypedAst.Def
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.api.lsp.Position
import ca.uwaterloo.flix.language.ast.TypedAst.Effect
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast.TypedAst.Constraint
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern
import ca.uwaterloo.flix.language.ast.TypedAst.StructField


object Visitors {
  // For now, visit is a placeholder. In the end we'll need some way
  // to supply visit functions for all types of elements at this top level
  def visitRoot(root: Root, visit: Root => Unit, accept: SourceLocation => Boolean): Unit = {


    root.defs.map{ case (_, defn) => {
      val insideDefn = accept(defn.spec.loc) || accept(defn.exp.loc) || accept(defn.sym.loc)
      if (insideDefn) {
        visitDef(defn, ???, accept)
      }
    }}

    root.effects.map{ case (_, eff) => {
      val insideEff = accept(eff.loc) || eff.ann.annotations.exists(ann => accept(ann.loc)) || accept(eff.sym.loc)
      if (insideEff) {
        visitEffect(eff, ???, accept)
      }
    }}

    // root.entryPoint.map{ case v => visitEntryPoint(visit, accept)(v) }
    // root.enums.map{ case (_, v) => visitEnum(???, ???)(v) };
    // root.instances.map { case (_, v) => visitInstance(???, ???)(v) };
    // root.modules.map { case (_, v) => visitModule(???, ???)(v) };
    // root.restrictableEnums.map { case (_, v) => visitRestrictable(???, ???)(v) }; //experimental, prob should be removed
    // root.sigs.map{ case (_, v) => visitSig(???, ???)(v) };
    // root.structs.map{ case (_, v) => visitStruct(???, ???)(v) };
    // root.traitEnv.map{ case (_, v) => visitTraitEnv(???, ???)(v) };
    // root.traits.map{ case (_, v) =>  visitTrait(???, ???)(v) };
    // root.typeAliases.map{ case (_, v) => visitTypeAlias(???, ???)(v) };
    // root.uses.map{ case (_, v) => visitUse(???, ???)(v) };
  }

  def inside(uri: String, pos: Position)(loc: SourceLocation): Boolean = {
    val posLine = pos.line + 1
    val posCol = pos.character + 1

    // sp1 and sp2, by invariant, has the same source, so we can use either
    (uri == loc.sp1.source.name) &&
    (posLine >= loc.beginLine) &&
    (posLine <= loc.endLine) &&
    !(posLine == loc.beginLine && posCol < loc.beginCol) &&
    !(posLine == loc.endLine && posCol > loc.endCol)
  }

  def inside(loc1: SourceLocation, loc2: SourceLocation): Boolean = {
    loc1.source == loc2.source &&
    (loc1.beginLine >= loc2.beginLine) &&
    (loc1.endLine <= loc2.endLine) &&
    !(loc2.beginLine == loc1.beginLine && loc2.beginCol > loc1.beginCol) &&
    !(loc1.endLine == loc2.endLine && loc1.endCol > loc2.endCol)
  }

  def visitEffect(eff: Effect, visit: Effect => Unit, accept: SourceLocation => Boolean): Unit = {
    visit(eff)
    ???
  }

  def visitDef(defn: Def, visit: Def => Unit, accept: SourceLocation => Boolean): Unit = {
    visit(defn)
    if (accept(defn.spec.loc)) {
      ???
    }

    if (accept(defn.exp.loc)) {
      visitExpr(defn.exp, ???, accept)
    }
  }

  def visitExpr(expr: Expr, visit: Expr => Unit, accept: SourceLocation => Boolean): Unit = {
    // TODO: handle mutually recursive calls to non expressions in expressions (fx annotations)

    visit(expr)
    expr match {
      case Expr.Cst(cst, tpe, loc) => ()
      case Expr.Var(sym, tpe, loc) => ()
      case Expr.Def(sym, tpe, loc) => ()
      case Expr.Sig(sym, tpe, loc) => ()
      case Expr.Hole(sym, tpe, loc) => ()
      case Expr.HoleWithExp(exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.OpenAs(symUse, exp, tpe, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.Use(sym, alias, exp, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.Lambda(fparam, exp, tpe, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.Apply(exp, exps, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
        exps.foreach(e => visitExpr(e, visit, accept))
      }
      case Expr.Unary(sop, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.Region(tpe, loc) => ()
      case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
        if (accept(exp3.loc)) { visitExpr(exp3, visit, accept) }
      }
      case Expr.Stm(exp1, exp2, _, _, _) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.Discard(exp, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.Match(exp, rules, tpe, eff, loc) => {
        rules.foreach(rule => {
          if (accept(rule.exp.loc)) { visitExpr(rule.exp, visit, accept) }
          rule.guard.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
          visitPattern(rule.pat, ???, accept)
        })
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.TypeMatch(exp, rules, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => {
        // Very limited hover info since feature is experimental
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
        rules.foreach(rule => visitExpr(rule.exp, visit, accept))
      }
      case Expr.Tag(sym, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.Tuple(exps, tpe, eff, loc) => {
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
      }
      case Expr.RecordEmpty(tpe, loc) => ()
      case Expr.RecordSelect(exp, label, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.RecordRestrict(label, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.ArrayLit(exps, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
      }
      case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
        if (accept(exp3.loc)) { visitExpr(exp3, visit, accept) }
      }
      case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.ArrayLength(exp, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.StructNew(sym, fields, region, tpe, eff, loc) => {
        fields.foreach{ case (_, e) => { if (accept(e.loc)) { visitExpr(e, visit, accept) } }}
        if (accept(region.loc)) { visitExpr(region, visit, accept) }
      }
      case Expr.StructGet(exp, sym, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.VectorLit(exps, tpe, eff, loc) => {
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
      }
      case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.VectorLength(exp, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.Ascribe(exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.InstanceOf(exp, clazz, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.CheckedCast(cast, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.UncheckedCast(exp, decalredType, declaredEff, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.Without(exp, effUse, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.TryCatch(exp, rules, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
        rules.foreach(rule => if (accept(rule.exp.loc)) { visitExpr(rule.exp, visit, accept)} )
      }
      case Expr.Throw(exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
        rules.foreach(rule => if (accept(rule.exp.loc)) { visitExpr(rule.exp, visit, accept)} )
      }
      case Expr.Do(op, exps, tpe, eff, loc) => {
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
      }
      case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => {
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
      }
      case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
      }
      case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => {
        exps.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
      }
      case Expr.GetField(field, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.GetStaticField(field, tpe, eff, loc) => ()
      case Expr.PutStaticField(field, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept) }
      }
      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ()
      case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.GetChannel(exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.SelectChannel(rules, default, tpe, eff, loc) => {
        rules.foreach(rule => {
          if (accept(rule.chan.loc)) { visitExpr(rule.chan, visit, accept) }
          if (accept(rule.exp.loc)) { visitExpr(rule.exp, visit, accept)}
        })

        default.foreach(e => if (accept(e.loc)) { visitExpr(e, visit, accept) })
      }
      case Expr.Spawn(exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.ParYield(frags, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
        frags.foreach(frag => {
          if (accept(frag.exp.loc)) { visitExpr(frag.exp, visit, accept)}
          // TODO: visit frag.pat (pattern)
        })
      }
      case Expr.Lazy(exp, tpe, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.Force(exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.FixpointConstraintSet(cs, tpe, loc) => {
        cs.foreach(con => visitConstraint(con, ???, accept))
      }
      case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => {
        if (accept(exp1.loc)) { visitExpr(exp1, visit, accept) }
        if (accept(exp2.loc)) { visitExpr(exp2, visit, accept) }
      }
      case Expr.FixpointSolve(exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.FixpointInject(exp, pred, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.FixpointProject(pred, exp, tpe, eff, loc) => {
        if (accept(exp.loc)) { visitExpr(exp, visit, accept)}
      }
      case Expr.Error(m, tpe, eff) => ()
    }
  }

  def visitConstraint(con: Constraint, visit: Constraint => Unit, accept: SourceLocation => Boolean): Unit = ???

  def visitPattern(pat: Pattern, visit: Pattern => Unit, accept: SourceLocation => Boolean): Unit = ???
}
