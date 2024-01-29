package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Level, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.constraintgeneration.TypingConstraint.Provenance
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypingContext {
  var constrs: ListBuffer[TypingConstraint] = ListBuffer.empty
  var renv: RigidityEnv = RigidityEnv.empty
  var level: Level = Level.Top
  var region: Option[Symbol.KindedTypeVarSym] = None
  var nest: mutable.Stack[(Option[Symbol.KindedTypeVarSym], ListBuffer[TypingConstraint])] = mutable.Stack.empty


  /**
    * Generates constraints unifying the given types.
    */
  def unifyTypeM(tpe1: Type, tpe2: Type, loc: SourceLocation): Unit = {
    constrs.append(TypingConstraint.Equality(tpe1, tpe2, Provenance.Match, loc))
  }

  /**
    * Generates constraints unifying the given types.
    */
  def unifyAllTypesM(tpes: List[Type], kind: Kind, loc: SourceLocation)(implicit level: Level, flix: Flix): Type = {
    tpes match {
      case tpe1 :: rest =>
        rest.foreach(unifyTypeM(tpe1, _, loc))
        tpe1
      case Nil => Type.freshVar(kind, loc.asSynthetic)
    }
  }

  /**
    * Generates constraints expecting the given type arguments to unify.
    */
  def expectTypeArguments(sym: Symbol, expectedTypes: List[Type], actualTypes: List[Type], actualLocs: List[SourceLocation], loc: SourceLocation)(implicit flix: Flix): Unit = {
    expectedTypes.zip(actualTypes).zip(actualLocs).zipWithIndex.foreach {
      case (((expectedType, actualType), loc), index) =>
        val argNum = index + 1
        val prov = Provenance.ExpectLeftArgument(sym, argNum)
        constrs.addOne(TypingConstraint.Equality(expectedType, actualType, prov, loc))
    }
  }

  /**
    * Generates constraints unifying the given types.
    */
  def unifyType3M(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation): Unit = {
    unifyTypeM(tpe1, tpe2, loc)
    unifyTypeM(tpe1, tpe3, loc)
  }

  /**
    * Generates constraints unifying the given effects.
    */
  // TODO ASSOC-TYPES this should actually do something
  def unifyEffM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit flix: Flix): Unit = {
    unifyTypeM(tpe1, tpe2, loc)
  }

  /**
    * Generates constraints expecting the given types to unify.
    */
  def expectTypeM(expected: Type, actual: Type, loc: SourceLocation): Unit = {
    constrs.append(TypingConstraint.Equality(expected, actual, Provenance.ExpectLeft, loc))
  }

  /**
    * Generates constraints expecting the given types to unify, binding them to the bound type.
    */
  // TODO ASSOC-TYPES what does this do?
  def expectTypeBindM(expected: Type, actual: Type, bind: Type, loc: SourceLocation): Unit = {
    expectTypeM(expected, actual, loc)
    unifyTypeM(expected, bind, loc)
  }

  /**
    * Generates constraints unifying the given Booleans.
    */
  // TODO ASSOC-TYPES this should actually do something
  def unifyBoolM(tpe1: Type, tpe2: Type, loc: SourceLocation): Unit = {
    unifyTypeM(tpe1, tpe2, loc)
  }

  /**
    * Adds the given class constraints to the context.
    */
  def addTypeConstraintsM(tconstrs0: List[Ast.TypeConstraint], loc: SourceLocation): Unit = {
    val tconstrs = tconstrs0.map {
      case Ast.TypeConstraint(head, arg, _) => TypingConstraint.Class(head.sym, arg, loc)
    }
    constrs.addAll(tconstrs)
  }

  /**
    * Marks the given type variable as rigid in the context.
    */
  def rigidifyM(sym: Symbol.KindedTypeVarSym): Unit = {
    renv = renv.markRigid(sym)
  }

  /**
    * Replaces every occurrence of the effect symbol `sym` with pure in `eff`.
    *
    * Note: Does not work for polymorphic effects.
    */
  def purifyEff(sym: Symbol.EffectSym, eff: Type): Type = {
    def visit(t: Type): Type = t match {
      case Type.Var(_, _) => t
      case Type.Cst(tc, _) => tc match {
        case TypeConstructor.Effect(sym2) if sym == sym2 => Type.Pure
        case _ => t
      }
      case Type.Apply(tpe1, tpe2, loc) => Type.Apply(visit(tpe1), visit(tpe2), loc)
      case Type.Alias(cst, _, tpe, _) => visit(tpe)
      case Type.AssocType(cst, arg, kind, loc) => Type.AssocType(cst, visit(arg), kind, loc)
    }

    visit(eff)
//    ??? // MATT need to replicate enterRegion and exitRegion stuff
  }
  /**
    * Enters a new region.
    */
  def enterRegionM(sym: Symbol.KindedTypeVarSym): Unit = {
    // save the info from the parent region
    nest.push((region, constrs))
    renv = renv.markRigid(sym)
    level = level.incr
    region = Some(sym)
    constrs = ListBuffer.empty
  }

  /**
    * Exits a region, unifying the external effect with a purified version of the internal effect.
    */
  def exitRegionM(externalEff1: Type, internalEff2: Type, loc: SourceLocation): Unit = {
    val constr = region match {
      case None => throw InternalCompilerException("unexpected missing region", loc)
      case Some(r) => TypingConstraint.Purification(r, externalEff1, internalEff2, level, Provenance.Match, constrs.toList, loc)
    }

    val (parReg, parConstrs) = nest.pop()
    region = parReg
    constrs = parConstrs
    level = level.decr
    constrs.append(constr)
  }
}
