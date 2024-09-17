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


object Visitors {
  def visitRoot(visit: Entity => Unit, accept: SourceLocation => Boolean)(root: Root): Unit = {
    // visit(Entity.Root(root))?

    root.defs.map{ case (_, defn) => {
      if (inside(accept, defn)) {
        visitDef(visit, accept)(defn)
      }
    }}

    root.effects.map{ case (_, v) => {
      if (inside(accept, v)) {
        visitEffect(visit, accept)(v)
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

  def inside(other: SourceLocation => Boolean, defn: Def): Boolean = {
    other(defn.spec.loc) || other(defn.exp.loc) || other(defn.sym.loc)
  }

  def inside(other: SourceLocation => Boolean, expr: Expr): Boolean = other(expr.loc)

  def inside(other: SourceLocation => Boolean, effect: Effect): Boolean = {
    other(effect.loc) || effect.ann.annotations.exists(ann => inside(other, ann)) || other(effect.sym.loc)
  }

  def inside(other: SourceLocation => Boolean, ann: Annotation): Boolean = other(ann.loc)

  def inside(loc1: SourceLocation, loc2: SourceLocation): Boolean = {
    loc1.source == loc2.source &&
    (loc1.beginLine >= loc2.beginLine) &&
    (loc1.endLine <= loc2.endLine) &&
    !(loc2.beginLine == loc1.beginLine && loc2.beginCol > loc1.beginCol) &&
    !(loc1.endLine == loc2.endLine && loc1.endCol > loc2.endCol)
  }

  def visitEffect(visit: Entity => Unit, accept: SourceLocation => Boolean)(effect: Effect): Unit = {
    visit(Entity.Effect(effect))
    ???
  }

  def visitDef(visit: Entity => Unit, accept: SourceLocation => Boolean)(defn: Def): Unit = {
    visit(Entity.Def(defn))
    if (accept(defn.spec.loc)) {
      ???
    }

    if (inside(accept, defn.exp)) {
      visitExpr(exp => visit(Entity.Exp(exp)), exp => inside(accept, defn))(defn.exp)
    }
  }

  def visitExpr(visit: Expr => Unit, accept: Expr => Boolean)(expr: Expr): Unit = {
    val recur = visitExpr(visit, accept)
    expr match {
      case Expr.Cst(_, _, _) => visit(expr)
      case Expr.Var(_, _, _) => visit(expr)
      case Expr.Def(sym, tpe, loc) => visit(expr)
      case Expr.Sig(sym, tpe, loc) => visit(expr)
      case Expr.Hole(sym, tpe, loc) => visit(expr)
      case Expr.HoleWithExp(exp, tpe, eff, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
      }
      case Expr.OpenAs(symUse, exp, tpe, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
      }
      case Expr.Use(sym, alias, exp, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
      }
      case Expr.Lambda(fparam, exp, tpe, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
      }
      case Expr.Apply(exp, exps, tpe, eff, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
        exps.foreach(recur)
      }
      case Expr.Unary(sop, exp, tpe, eff, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
      }
      case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
        visit(expr)
        if (accept(exp1)) { recur(exp1) }
        if (accept(exp2)) { recur(exp2) }
      case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
        visit(expr)
        if (accept(exp1)) { recur(exp1) }
        if (accept(exp2)) { recur(exp2) }
      case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => {
        visit(expr)
        if (accept(exp1)) { recur(exp1) }
        if (accept(exp2)) { recur(exp2) }
      }
      case Expr.Region(tpe, loc) => visit(expr)
      case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => visit(expr)
      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visit(expr)
      case Expr.Stm(exp1, exp2, _, _, _) => {
        visit(expr)
        if (accept(exp1)) { recur(exp1) }
        if (accept(exp2)) { recur(exp2) }
      }
      case Expr.Discard(exp, eff, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
      }
      case Expr.Match(exp, rules, tpe, eff, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
      }
      case Expr.TypeMatch(exp, rules, tpe, eff, loc) => {
        visit(expr)
        if (accept(exp)) { recur(exp) }
      }
      case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => ???
      case Expr.Tag(sym, exp, tpe, eff, loc) => ???
      case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => ???
      case Expr.Tuple(exps, tpe, eff, loc) => ???
      case Expr.RecordEmpty(tpe, loc) => ???
      case Expr.RecordSelect(exp, label, tpe, eff, loc) => ???
      case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => ???
      case Expr.RecordRestrict(label, exp, tpe, eff, loc) => ???
      case Expr.ArrayLit(exps, exp, tpe, eff, loc) => ???
      case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => ???
      case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => ???
      case Expr.ArrayLength(exp, eff, loc) => ???
      case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => ???
      case Expr.StructNew(sym, fields, region, tpe, eff, loc) => ???
      case Expr.StructGet(exp, sym, tpe, eff, loc) => ???
      case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) => ???
      case Expr.VectorLit(exps, tpe, eff, loc) => ???
      case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => ???
      case Expr.VectorLength(exp, loc) => ???
      case Expr.Ascribe(exp, tpe, eff, loc) => ???
      case Expr.InstanceOf(exp, clazz, loc) => ???
      case Expr.CheckedCast(cast, exp, tpe, eff, loc) => ???
      case Expr.UncheckedCast(exp, decalredType, declaredEff, tpe, eff, loc) => ???
      case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => ???
      case Expr.Without(exp, effUse, tpe, eff, loc) => ???
      case Expr.TryCatch(exp, rules, tpe, eff, loc) => ???
      case Expr.Throw(exp, tpe, eff, loc) => ???
      case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => ???
      case Expr.Do(op, exps, tpe, eff, loc) => ???
      case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => ???
      case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => ???
      case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => ???
      case Expr.GetField(field, exp, tpe, eff, loc) => ???
      case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => ???
      case Expr.GetStaticField(field, tpe, eff, loc) => ???
      case Expr.PutStaticField(field, exp, tpe, eff, loc) => ???
      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ???
      case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => ???
      case Expr.GetChannel(exp, tpe, eff, loc) => ???
      case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => ???
      case Expr.SelectChannel(rules, default, tpe, eff, loc) => ???
      case Expr.Spawn(exp1, exp2, tpe, eff, loc) => ???
      case Expr.ParYield(frags, exp, tpe, eff, loc) => ???
      case Expr.Lazy(exp, tpe, loc) => ???
      case Expr.Force(exp, tpe, eff, loc) => ???
      case Expr.FixpointConstraintSet(cs, tpe, loc) => ???
      case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => ???
      case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => ???
      case Expr.FixpointSolve(exp, tpe, eff, loc) => ???
      case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => ???
      case Expr.FixpointInject(exp, pred, tpe, eff, loc) => ???
      case Expr.FixpointProject(pred, exp, tpe, eff, loc) => ???
      case Expr.Error(m, tpe, eff) => ???  
    }
  }
}
