package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.LoweredAst.{Expression, Pattern, RelationalChoicePattern}
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.{Ast, LoweredAst, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object MonomorphEnums {

  /**
    * Holds the mutable data used throughout monomorphization.
    */
  private class Context() {

    /**
      * A map from a symbol and a concrete type to the fresh symbol for the
      * specialized version of that enum.
      *
      * For example, if the enum:
      *
      * -   def MyEnum[a, b]{ case MyCase(a, b) }
      *
      * has been specialized w.r.t. to `Int` and `String` then this map will contain an entry:
      *
      * -   (MyEnum, MyEnum[Int, String]) -> MyEnum$42
      *
      * Like shown here, the type should be pre enum specialization and the enum
      * symbols should be un-specialized, i.e. present in root.enums.
      */
    val enum2enum: mutable.Map[(Symbol.EnumSym, Type), Symbol.EnumSym] = mutable.Map.empty

    /**
      * A map used to collect specialized definitions.
      */
    val specializedEnums: mutable.Map[Symbol.EnumSym, LoweredAst.Enum] = mutable.Map.empty
  }

  def run(root: LoweredAst.Root)(implicit flix: Flix): LoweredAst.Root = flix.phase("MonomorphEnums"){
    // Two assumption:
    // - All typeclass information have been transformed into defs - this
    //   phase only looks at types and expressions in defs.
    // - All the following types have been removed:
    //   - Type variables
    //   - Type aliases
    //   - Associated types
    // - In schemas these are unused
    //   - tconstrs
    //   - econstrs

    // monomorphization works by finding ground enum types in expressions and
    // types.
    // When such an enum is found, its symbol is bound in `ctx.enum2enum` and
    // the is specialized and put into `ctx.specializedEnums`. This process
    // might be recursive which is why the symbol is put into enum2enum before
    // the work is actually done.

    implicit val r: LoweredAst.Root = root

    implicit val ctx: Context = new Context()

    val defs = for ((sym, defn) <- root.defs) yield {
      val spec0 = defn.spec
      val spec = LoweredAst.Spec(
        spec0.doc,
        spec0.ann,
        spec0.mod,
        spec0.tparams,
        spec0.fparams.map(visitFormalParam),
        visitScheme(spec0.declaredScheme),
        visitType(spec0.retTpe),
        visitType(spec0.pur),
        spec0.tconstrs, spec0.loc
      )
      val impl = LoweredAst.Impl(visitExp(defn.impl.exp), visitScheme(defn.impl.inferredScheme))
      (sym, LoweredAst.Def(sym, spec, impl))
    }

    root.copy(defs = defs, enums = ctx.specializedEnums.toMap)
  }

  private def visitExp(exp: LoweredAst.Expression)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.Expression = exp match {
    case Expression.Cst(cst, tpe, loc) =>
      Expression.Cst(cst, visitType(tpe), loc)
    case Expression.Wild(tpe, loc) =>
      Expression.Wild(visitType(tpe), loc)
    case Expression.Var(sym, tpe, loc) =>
      Expression.Var(sym, visitType(tpe), loc)
    case Expression.Def(sym, tpe, loc) =>
      Expression.Def(sym, visitType(tpe), loc)
    case Expression.Sig(sym, tpe, loc) =>
      Expression.Sig(sym, visitType(tpe), loc)
    case Expression.Hole(sym, tpe, loc) =>
      Expression.Hole(sym, visitType(tpe), loc)
    case Expression.Lambda(fparam, exp, tpe, loc) =>
      Expression.Lambda(visitFormalParam(fparam), visitExp(exp), visitType(tpe), loc)
    case Expression.Apply(exp, exps, tpe, pur, loc) =>
      Expression.Apply(visitExp(exp), exps.map(visitExp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Unary(sop, exp, tpe, pur, loc) =>
      Expression.Unary(sop, visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Binary(sop, exp1, exp2, tpe, pur, loc) =>
      Expression.Binary(sop, visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Let(sym, mod, exp1, exp2, tpe, pur, loc) =>
      Expression.Let(sym, mod, visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, loc) =>
      Expression.LetRec(sym, mod, visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Region(tpe, loc) =>
      Expression.Region(visitType(tpe), loc)
    case Expression.Scope(sym, regionVar, exp, tpe, pur, loc) =>
      // TODO: the regionVar??
      Expression.Scope(sym, regionVar, visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.ScopeExit(exp1, exp2, tpe, pur, loc) =>
      Expression.ScopeExit(visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, loc) =>
      Expression.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Stm(exp1, exp2, tpe, pur, loc) =>
      Expression.Stm(visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Discard(exp, pur, loc) =>
      Expression.Discard(visitExp(exp), visitEffectType(pur), loc)
    case Expression.Match(exp, rules, tpe, pur, loc) =>
      val newRules = rules.map{
        case LoweredAst.MatchRule(pat, guard, exp) =>
          LoweredAst.MatchRule(visitPat(pat), guard.map(visitExp), visitExp(exp))
      }
      Expression.Match(visitExp(exp), newRules, visitType(tpe), visitEffectType(pur), loc)
    case Expression.TypeMatch(exp, rules, tpe, pur, loc) =>
      val newRules = rules.map{
        case LoweredAst.MatchTypeRule(sym, tpe, exp) =>
          LoweredAst.MatchTypeRule(sym, visitType(tpe), visitExp(exp))
      }
      Expression.TypeMatch(visitExp(exp), newRules, visitType(tpe), visitEffectType(pur), loc)
    case Expression.RelationalChoose(exps, rules, tpe, pur, loc) =>
      val newRules = rules.map {
        case LoweredAst.RelationalChoiceRule(pat, exp) =>
          val newPat = pat.map {
            case RelationalChoicePattern.Wild(loc) => RelationalChoicePattern.Wild(loc)
            case RelationalChoicePattern.Absent(loc) => RelationalChoicePattern.Absent(loc)
            case RelationalChoicePattern.Present(sym, tpe, loc) => RelationalChoicePattern.Present(sym, visitType(tpe), loc)
          }
          LoweredAst.RelationalChoiceRule(newPat, visitExp(exp))
      }
      Expression.RelationalChoose(exps.map(visitExp), newRules, visitType(tpe), visitEffectType(pur), loc)
    case Expression.Tag(sym, exp, tpe, pur, loc) =>
      val freshEnumSym = specializeEnum(sym.sym.enumSym, tpe.typeArguments, tpe.loc)
      val freshCaseSym = Ast.CaseSymUse(new Symbol.CaseSym(freshEnumSym, sym.sym.name, sym.sym.loc), sym.loc)
      Expression.Tag(freshCaseSym, visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Tuple(elms, tpe, pur, loc) =>
      Expression.Tuple(elms.map(visitExp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.RecordEmpty(tpe, loc) =>
      Expression.RecordEmpty(visitType(tpe), loc)
    case Expression.RecordSelect(exp, field, tpe, pur, loc) =>
      Expression.RecordSelect(visitExp(exp), field, visitType(tpe), visitEffectType(pur), loc)
    case Expression.RecordExtend(field, value, rest, tpe, pur, loc) =>
      Expression.RecordExtend(field, visitExp(value), visitExp(rest), visitType(tpe), visitEffectType(pur), loc)
    case Expression.RecordRestrict(field, rest, tpe, pur, loc) =>
      Expression.RecordRestrict(field, visitExp(rest), visitType(tpe), visitEffectType(pur), loc)
    case Expression.ArrayLit(exps, exp, tpe, pur, loc) =>
      Expression.ArrayLit(exps.map(visitExp), visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, loc) =>
      Expression.ArrayNew(visitExp(exp1), visitExp(exp2), visitExp(exp3), visitType(tpe), visitEffectType(pur), loc)
    case Expression.ArrayLoad(base, index, tpe, pur, loc) =>
      Expression.ArrayLoad(visitExp(base), visitExp(index), visitType(tpe), visitEffectType(pur), loc)
    case Expression.ArrayLength(base, pur, loc) =>
      Expression.ArrayLength(visitExp(base), visitEffectType(pur), loc)
    case Expression.ArrayStore(base, index, elm, pur, loc) =>
      Expression.ArrayStore(visitExp(base), visitExp(index), visitExp(elm), visitEffectType(pur), loc)
    case Expression.VectorLit(exps, tpe, pur, loc) =>
      Expression.VectorLit(exps.map(visitExp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.VectorLoad(exp1, exp2, tpe, pur, loc) =>
      Expression.VectorLoad(visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.VectorLength(exp, loc) =>
      Expression.VectorLength(visitExp(exp), loc)
    case Expression.Ref(exp1, exp2, tpe, pur, loc) =>
      Expression.Ref(visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Deref(exp, tpe, pur, loc) =>
      Expression.Deref(visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Assign(exp1, exp2, tpe, pur, loc) =>
      Expression.Assign(visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Ascribe(exp, tpe, pur, loc) =>
      Expression.Ascribe(visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.InstanceOf(exp, clazz, loc) =>
      Expression.InstanceOf(visitExp(exp), clazz, loc)
    case Expression.Cast(exp, declaredType, declaredPur, tpe, pur, loc) =>
      Expression.Cast(visitExp(exp), declaredType.map(visitType), declaredPur.map(visitType), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Without(exp, effUse, tpe, pur, loc) =>
      Expression.Without(visitExp(exp), effUse, visitType(tpe), visitEffectType(pur), loc)
    case Expression.TryCatch(exp, rules, tpe, pur, loc) =>
      val newRules = rules.map{
        case LoweredAst.CatchRule(sym, clazz, exp) =>
          LoweredAst.CatchRule(sym, clazz, visitExp(exp))
      }
      Expression.TryCatch(visitExp(exp), newRules, visitType(tpe), visitEffectType(pur), loc)
    case Expression.TryWith(exp, effUse, rules, tpe, pur, loc) =>
      val newRules = rules.map{
        case LoweredAst.HandlerRule(op, fparams, exp) =>
          LoweredAst.HandlerRule(op, fparams.map(visitFormalParam), visitExp(exp))
      }
      Expression.TryWith(visitExp(exp), effUse, newRules, visitType(tpe), visitEffectType(pur), loc)
    case Expression.Do(op, exps, pur, loc) =>
      Expression.Do(op, exps.map(visitExp), visitEffectType(pur), loc)
    case Expression.Resume(exp, tpe, loc) =>
      Expression.Resume(visitExp(exp), visitType(tpe), loc)
    case Expression.InvokeConstructor(constructor, args, tpe, pur, loc) =>
      Expression.InvokeConstructor(constructor, args.map(visitExp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.InvokeMethod(method, exp, args, tpe, pur, loc) =>
      Expression.InvokeMethod(method, visitExp(exp), args.map(visitExp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.InvokeStaticMethod(method, args, tpe, pur, loc) =>
      Expression.InvokeStaticMethod(method, args.map(visitExp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.GetField(field, exp, tpe, pur, loc) =>
      Expression.GetField(field, visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.PutField(field, exp1, exp2, tpe, pur, loc) =>
      Expression.PutField(field, visitExp(exp1), visitExp(exp1), visitType(tpe), visitEffectType(pur), loc)
    case Expression.GetStaticField(field, tpe, pur, loc) =>
      Expression.GetStaticField(field, visitType(tpe), visitEffectType(pur), loc)
    case Expression.PutStaticField(field, exp, tpe, pur, loc) =>
      Expression.PutStaticField(field, visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
    case Expression.NewObject(name, clazz, tpe, pur, methods, loc) =>
      val newMethods = methods.map{
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, pur, loc) =>
          LoweredAst.JvmMethod(ident, fparams.map(visitFormalParam), visitExp(exp), visitType(retTpe), visitEffectType(pur), loc)
      }
      Expression.NewObject(name, clazz, visitType(tpe), visitEffectType(pur), newMethods, loc)
    case Expression.Spawn(exp1, exp2, tpe, pur, loc) =>
      Expression.Spawn(visitExp(exp1), visitExp(exp2), visitType(tpe), visitEffectType(pur), loc)
    case Expression.Lazy(exp, tpe, loc) =>
      Expression.Lazy(visitExp(exp), visitType(tpe), loc)
    case Expression.Force(exp, tpe, pur, loc) =>
      Expression.Force(visitExp(exp), visitType(tpe), visitEffectType(pur), loc)
  }

  private def visitPat(pat: LoweredAst.Pattern)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.Pattern = pat match {
    case Pattern.Wild(tpe, loc) => Pattern.Wild(visitType(tpe), loc)
    case Pattern.Var(sym, tpe, loc) => Pattern.Var(sym, visitType(tpe), loc)
    case Pattern.Cst(cst, tpe, loc) => Pattern.Cst(cst, visitType(tpe), loc)
    case Pattern.Tag(sym, tagPat, tpe, loc) =>
      val newTagPat = visitPat(tagPat)
      val freshEnumSym = specializeEnum(sym.sym.enumSym, tpe.typeArguments, tpe.loc)
      val freshCaseSym = Ast.CaseSymUse(new Symbol.CaseSym(freshEnumSym, sym.sym.name, sym.sym.loc), sym.loc)
      Pattern.Tag(freshCaseSym, newTagPat, visitType(tpe), loc)
    case Pattern.Tuple(elms, tpe, loc) => Pattern.Tuple(elms.map(visitPat), visitType(tpe), loc)
  }

  private def visitType(tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Type = {
    def visitInner(tpe0: Type): Type = tpe0.baseType match {
      case Type.Cst(TypeConstructor.Enum(sym, _), loc) => // enum
        val args = tpe0.typeArguments
        val freshSym = specializeEnum(sym, args, loc)
        Type.mkEnum(freshSym, Nil, loc)
      case _ => tpe0 match { // non-enum
        case Type.Cst(tc, loc) => Type.Cst(tc, loc)
        case Type.Apply(tpe1, tpe2, loc) => Type.Apply(visitInner(tpe1), visitInner(tpe2), loc)
        case Type.Var(sym, loc) => throw InternalCompilerException(s"Unexpected type var: '$sym'", loc)
        case Type.Alias(cst, _, _, loc) => throw InternalCompilerException(s"Unexpected type alias: '${cst.sym}'", loc)
        case Type.AssocType(cst, _, _, loc) => throw InternalCompilerException(s"Unexpected associated type: '${cst.sym}'", loc)
      }
    }
    visitInner(eraseAliases(tpe))
  }

  private def visitEffectType(tpe: Type): Type = tpe

  private def visitFormalParam(p: LoweredAst.FormalParam)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): LoweredAst.FormalParam = {
    val LoweredAst.FormalParam(sym, mod, tpe, src, loc) = p
    LoweredAst.FormalParam(sym, mod, visitType(tpe), src, loc)
  }

  private def visitScheme(sc: Scheme)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Scheme = {
    val Scheme(quantifiers, tconstrs, econstrs, base) = sc
    Scheme(quantifiers, tconstrs, econstrs, visitType(base))
  }

  private def specializeEnum(sym: Symbol.EnumSym, args0: List[Type], loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.EnumSym = {
    val args = args0.map(eraseAliases)
    if (args.nonEmpty) println(args)
    // check assumptions
    args.foreach(t => if (t.typeVars.nonEmpty) throw InternalCompilerException(s"Unexpected type var: '$sym'", loc))
    // assemble enum type (e.g. `List[Int32]`)
    val tpe = Type.mkEnum(sym, args, loc)
    // reuse specialization if possible
    ctx.enum2enum.get((sym, tpe)) match {
      case Some(freshSym) =>
        // specialization was already done
        freshSym
      case None =>
        // reuse the existing symbol is the enum is non-parametric
        val freshSym = if (args.isEmpty) sym else Symbol.freshEnumSym(sym)
        // insert the symbol for reuse
        // (inserted before the work is done to avoid infinite recursion)
        ctx.enum2enum.put((sym, tpe), freshSym)

        // do the specialization
        // The enum is parametric on its type parameters, so we must instantiate
        // e.g. for List[a] and List[Int32] we substitute [a -> Int32]
        val e = root.enums(sym)
        val subst = Substitution(e.tparams.map(_.sym).zip(args).toMap)
        val cases = e.cases.map{
          case (caseSym, LoweredAst.Case(_, caseTpe, caseSc, caseLoc)) =>
            val freshCaseSym = new Symbol.CaseSym(freshSym, caseSym.name, caseSym.loc)
            val newCaseSc = Scheme(caseSc.quantifiers, caseSc.tconstrs, caseSc.econstrs, subst(caseSc.base))
            val caze = LoweredAst.Case(freshCaseSym, visitType(subst(caseTpe)), visitScheme(newCaseSc), caseLoc)
            (freshCaseSym, caze)
        }
        val freshEnum = LoweredAst.Enum(
          e.doc,
          e.ann,
          e.mod,
          freshSym,
          Nil,
          e.derives,
          cases,
          e.tpeDeprecated,
          e.loc
        )
        ctx.specializedEnums.put(freshSym, freshEnum)
        freshSym
    }
  }

}
