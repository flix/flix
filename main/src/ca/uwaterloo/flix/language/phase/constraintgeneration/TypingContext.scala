package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, LevelEnv, RigidityEnv, SourceLocation, Symbol, Type}

import scala.collection.mutable.ListBuffer

class TypingContext {
  val constrs: ListBuffer[TypingConstraint] = ListBuffer.empty
  var renv: RigidityEnv = RigidityEnv.empty
  var lenv: LevelEnv = LevelEnv.Top

  /**
    * Generates constraints unifying the given types.
    */
  def unifyTypeM(tpe1: Type, tpe2: Type, loc: SourceLocation): Unit = {
    constrs.append(TypingConstraint.Equality(tpe1, tpe2, lenv, loc))
  }

  /**
    * Generates constraints unifying the given types.
    */
  def unifyAllTypesM(tpes: List[Type], kind: Kind, loc: SourceLocation)(implicit flix: Flix): Type = {
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
  // TODO ASSOC-TYPES this should actually do something
  def expectTypeArguments(sym: Symbol, expectedTypes: List[Type], actualTypes: List[Type], actualLocs: List[SourceLocation], loc: SourceLocation)(implicit flix: Flix): Unit = {
    expectedTypes.zip(actualTypes).zip(actualLocs).foreach {
      case ((expectedType, actualType), loc) => expectTypeM(expectedType, actualType, loc)
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
  // TODO ASSOC-TYPES this should actually do something
  def expectTypeM(expected: Type, actual: Type, loc: SourceLocation): Unit = {
    unifyTypeM(expected, actual, loc)
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
      case Ast.TypeConstraint(head, arg, _) => TypingConstraint.Class(head.sym, arg, lenv, loc)
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
    * Enters the type variable's scope in the context.
    */
  def enterScopeM(sym: Symbol.KindedTypeVarSym): Unit = {
    lenv = lenv.enterScope(sym)
  }

  /**
    * Exits the type variable's scope in the context.
    */
  def exitScopeM(sym: Symbol.KindedTypeVarSym): Unit = {
    lenv = lenv.exitScope(sym)
  }
}
