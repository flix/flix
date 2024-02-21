/*
 * Copyright 2015-2023 Magnus Madsen, Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.KindedAst.Expr
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Level, Name, RigidityEnv, Scheme, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.constraintgeneration.{RestrictableChooseConstraintGeneration, SchemaConstraintGeneration, TypingConstraint, TypingContext}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

object ConstraintGeneration {

  case class PhaseResult(
                          defs: Map[Symbol.DefnSym, (List[TypingConstraint], Type, Type, RigidityEnv)],
                          sigs: Map[Symbol.SigSym, (List[TypingConstraint], Type, Type, RigidityEnv)],
                        )

  def run(root: KindedAst.Root)(implicit flix: Flix): PhaseResult = {
    val result = flix.subphase("ConstraintGeneration") {
      val defVal = ParOps.parMapValues(root.defs) {
        case defn =>
          implicit val context: TypingContext = new TypingContext
          implicit val r: KindedAst.Root = root
          val (tpe, eff) = visitExp(defn.exp)
          val constrs = context.constrs.toList
          val renv = context.renv
          (constrs, tpe, eff, renv)
      }

      // filter down signatures to those with implementations
      // and collect the rigid variables from the class
      val sigsWithImplIter = for {
        clazz <- root.classes.values
        rigid = List(clazz.tparam.sym)
        sig <- clazz.sigs.values
        exp <- sig.exp
      } yield (sig.sym -> (exp, rigid))
      val sigsWithImpl = sigsWithImplIter.toMap

      val sigVal = ParOps.parMapValues(sigsWithImpl) {
        case (exp, rigid) =>
          // TODO ASSOC-TYPES dedupe
          implicit val context: TypingContext = new TypingContext
          implicit val r: KindedAst.Root = root
          rigid.foreach(context.rigidifyM)
          val (tpe, eff) = visitExp(exp)
          val constrs = context.constrs.toList
          val renv = context.renv
          (constrs, tpe, eff, renv)
      }

      val lawsIter = for {
        clazz <- root.classes.values
        rigid = List(clazz.tparam.sym)
        law <- clazz.laws
      } yield (law.sym -> (law, rigid))
      val laws = lawsIter.toMap

      val lawVal = ParOps.parMapValues(laws) {
        case (defn, rigid) =>
          // TODO ASSOC-TYPES dedupe
          implicit val context: TypingContext = new TypingContext
          implicit val r: KindedAst.Root = root
          rigid.foreach(context.rigidifyM)
          val (tpe, eff) = visitExp(defn.exp)
          val constrs = context.constrs.toList
          val renv = context.renv
          (constrs, tpe, eff, renv)
      }

      val instDefsIter = for {
        insts <- root.instances.values
        inst <- insts
        rigid = inst.tpe.typeVars.map(_.sym)
        defn <- inst.defs
      } yield (defn.sym -> (defn, rigid))
      val instDefs = instDefsIter.toMap

      val instVal = ParOps.parMapValues(instDefs) {
        case (defn, rigid) =>
          // TODO ASSOC-TYPES dedupe
          implicit val context: TypingContext = new TypingContext
          implicit val r: KindedAst.Root = root
          rigid.foreach(context.rigidifyM)
          val (tpe, eff) = visitExp(defn.exp)
          val constrs = context.constrs.toList
          val renv = context.renv
          (constrs, tpe, eff, renv)
      }

      PhaseResult(defVal ++ instVal ++ lawVal, sigVal)
    }

    // report the constraints if directed
    if (flix.options.xprintconstraints) {
      var overall = ConstraintAnalysis.empty
      result.defs.foreach {
        case (sym, (tconstrs, tpe, eff, renv)) =>
          println(sym)
          //          val analysis = analyzeAll(tconstrs)
          //          overall = overall ++ analysis
          //          analysis.print()
          println("Type: " + tpe)
          println("Effect: " + eff)
          println("Rigid: " + renv.s.mkString(", "))
          println("Type constraints: " + tconstrs.mkString(", "))
          println()
      }

      println("===== ANALYSIS TOTALS =====")
      overall.print()
    }

    // return the result
    result
  }

  case object ConstraintAnalysis {
    def empty: ConstraintAnalysis = ConstraintAnalysis(0, 0, 0, 0, 0, 0, 0)
  }

  case class ConstraintAnalysis(totalEq: Int, totalClass: Int, bothCst: Int, bothVar: Int, maxSize: Int, classCst: Int, classVar: Int) {
    def ++(that: ConstraintAnalysis): ConstraintAnalysis = {
      ConstraintAnalysis(
        totalEq = this.totalEq + that.totalEq,
        totalClass = this.totalClass,
        bothCst = this.bothCst + that.bothCst,
        bothVar = this.bothVar + that.bothVar,
        maxSize = Integer.max(this.maxSize, that.maxSize),
        classCst = this.classCst + that.classCst,
        classVar = this.classVar + that.classVar,
      )
    }

    def print(): Unit = {
      println("Equal Constraints: " + totalEq)
      println("Class Constraints: " + totalClass)
      println("Both Constant: " + bothCst)
      println("Both Variable: " + bothVar)
      println("Maximum size: " + maxSize)
      println("Class Constant: " + classCst)
      println("Class Variable: " + classVar)
    }
  }

  def analyzeAll(constrs: List[TypingConstraint]): ConstraintAnalysis = {
    constrs.map(analyze).foldLeft(ConstraintAnalysis.empty)(_ ++ _)
  }

  def analyze(constr: TypingConstraint): ConstraintAnalysis = constr match {
    case TypingConstraint.Equality(tpe1: Type.Var, tpe2: Type.Var, prov, loc) => ConstraintAnalysis(
      totalEq = 1,
      totalClass = 0,
      bothCst = 0,
      bothVar = 1,
      maxSize = Integer.max(tpe1.size, tpe2.size),
      classCst = 0,
      classVar = 0
    )
    case TypingConstraint.Equality(tpe1: Type.Cst, tpe2: Type.Cst, prov, loc) => ConstraintAnalysis(
      totalEq = 1,
      totalClass = 0,
      bothCst = 1,
      bothVar = 0,
      maxSize = Integer.max(tpe1.size, tpe2.size),
      classCst = 0,
      classVar = 0
    )
    case TypingConstraint.Equality(tpe1, tpe2, prov, loc) => ConstraintAnalysis(
      totalEq = 1,
      totalClass = 0,
      bothCst = 0,
      bothVar = 0,
      maxSize = Integer.max(tpe1.size, tpe2.size),
      classCst = 0,
      classVar = 0
    )
    case TypingConstraint.Class(sym, tpe: Type.Var, loc) => ConstraintAnalysis(
      totalEq = 0,
      totalClass = 1,
      bothCst = 0,
      bothVar = 0,
      maxSize = tpe.size,
      classCst = 0,
      classVar = 1,
    )
    case TypingConstraint.Class(sym, tpe: Type.Cst, loc) => ConstraintAnalysis(
      totalEq = 0,
      totalClass = 1,
      bothCst = 0,
      bothVar = 0,
      maxSize = tpe.size,
      classCst = 1,
      classVar = 0,
    )
    case TypingConstraint.Class(sym, tpe, loc) => ConstraintAnalysis(
      totalEq = 0,
      totalClass = 1,
      bothCst = 0,
      bothVar = 0,
      maxSize = tpe.size,
      classCst = 0,
      classVar = 0,
    )
    case _ => ConstraintAnalysis.empty // MATT
  }

  /**
    * Generates constraints for the given expression.
    */
  def visitExp(exp0: KindedAst.Expr)(implicit c: TypingContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    // Make the context's level available
    // This is a def rather than a val because c is mutable.
    implicit def level: Level = c.level

    exp0 match {
      case Expr.Var(sym, loc) => (sym.tvar, Type.Pure)
      case Expr.Def(sym, tvar, loc) =>
        val defn = root.defs(sym)
        val (tconstrs, defTpe) = Scheme.instantiate(defn.spec.sc, loc.asSynthetic)
        c.unifyTypeM(tvar, defTpe, loc)
        c.addTypeConstraintsM(tconstrs, loc)
        val resTpe = defTpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Sig(sym, tvar, loc) =>
        val sig = root.classes(sym.clazz).sigs(sym)
        val (tconstrs, sigTpe) = Scheme.instantiate(sig.spec.sc, loc.asSynthetic)
        c.unifyTypeM(tvar, sigTpe, loc)
        c.addTypeConstraintsM(tconstrs, loc)
        val resTpe = sigTpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Hole(sym, tpe, loc) =>
        (tpe, Type.Pure)

      case Expr.HoleWithExp(exp, tvar, evar, loc) =>
        val (tpe, eff) = visitExp(exp)
        // effect type is AT LEAST the inner expression's effect
        val atLeastEff = Type.mkUnion(eff, Type.freshVar(Kind.Eff, loc.asSynthetic), loc.asSynthetic)
        c.unifyTypeM(atLeastEff, evar, loc)
        // result type is whatever is needed for the hole
        val resTpe = tvar
        val resEff = atLeastEff
        (resTpe, resEff)

      case e@Expr.OpenAs(symUse, exp, tvar, loc) => RestrictableChooseConstraintGeneration.visitOpenAs(e)

      case Expr.Use(sym, alias, exp, loc) =>
        visitExp(exp)

      case Expr.Cst(cst, loc) =>
        val resTpe = TypeReconstruction.constantType(cst)
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Apply(exp, exps, tvar, evar, loc) =>
        //
        // Determine if there is a direct call to a Def or Sig.
        //
        val knownTarget = exp match {
          case KindedAst.Expr.Def(sym, tvar2, loc2) =>
            // Case 1: Lookup the sym and instantiate its scheme.
            val defn = root.defs(sym)
            val (tconstrs1, declaredType) = Scheme.instantiate(defn.spec.sc, loc2.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar2, constrs1, declaredType))

          case KindedAst.Expr.Sig(sym, tvar2, loc2) =>
            // Case 2: Lookup the sym and instantiate its scheme.
            val sig = root.classes(sym.clazz).sigs(sym)
            val (tconstrs1, declaredType) = Scheme.instantiate(sig.spec.sc, loc2.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar2, constrs1, declaredType))

          case _ =>
            // Case 3: Unknown target.
            None
        }

        knownTarget match {
          case Some((sym, tvar2, constrs1, declaredType)) =>
            //
            // Special Case: We are applying a Def or Sig and we break apart its declared type.
            //
            val declaredEff = declaredType.arrowEffectType
            val declaredArgumentTypes = declaredType.arrowArgTypes
            val declaredResultType = declaredType.arrowResultType

            val (tpes, effs) = exps.map(visitExp).unzip
            c.expectTypeArguments(sym, declaredArgumentTypes, tpes, exps.map(_.loc), loc)
            c.unifyTypeM(tvar2, declaredType, loc)
            c.unifyTypeM(tvar, declaredResultType, loc)
            c.unifyEffM(evar, Type.mkUnion(declaredEff :: effs, loc), loc)
            val resTpe = tvar
            val resEff = evar
            (resTpe, resEff)

          case None =>
            //
            // Default Case: Apply.
            //
            val lambdaBodyType = Type.freshVar(Kind.Star, loc)
            val lambdaBodyEff = Type.freshVar(Kind.Eff, loc)
            val (tpe, eff) = visitExp(exp)
            val (tpes, effs) = exps.map(visitExp).unzip
            c.expectTypeM(tpe, Type.mkUncurriedArrowWithEffect(tpes, lambdaBodyEff, lambdaBodyType, loc), loc)
            c.unifyTypeM(tvar, lambdaBodyType, loc)
            c.unifyEffM(evar, Type.mkUnion(lambdaBodyEff :: eff :: effs, loc), loc)
            // TODO ASSOC-TYPES unbind?
            //            _ <- unbindVar(lambdaBodyType) // NB: Safe to unbind since the variable is not used elsewhere.
            //            _ <- unbindVar(lambdaBodyEff) // NB: Safe to unbind since the variable is not used elsewhere.
            val resTpe = tvar
            val resEff = evar
            (resTpe, resEff)
        }

      case Expr.Lambda(fparam, exp, loc) =>
        c.unifyTypeM(fparam.sym.tvar, fparam.tpe, loc)
        val (tpe, eff) = visitExp(exp)
        val resTpe = Type.mkArrowWithEffect(fparam.tpe, eff, tpe, loc)
        val resEff = Type.Pure
        (resTpe, resEff)

      case KindedAst.Expr.Unary(sop, exp, tvar, loc) => sop match {
        case SemanticOp.BoolOp.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectTypeBindM(expected = Type.Bool, actual = tpe, bind = tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Float32Op.Neg =>
          val (tpe, eff) = visitExp(exp)
          c.expectTypeBindM(expected = Type.Float32, actual = tpe, bind = tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Float64Op.Neg =>
          val (tpe, eff) = visitExp(exp)
          c.expectTypeBindM(expected = Type.Float64, actual = tpe, bind = tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Int8Op.Neg | SemanticOp.Int8Op.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectTypeBindM(expected = Type.Int8, actual = tpe, bind = tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Int16Op.Neg | SemanticOp.Int16Op.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectTypeBindM(expected = Type.Int16, actual = tpe, bind = tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Int32Op.Neg | SemanticOp.Int32Op.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectTypeBindM(expected = Type.Int32, actual = tpe, bind = tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Int64Op.Neg | SemanticOp.Int64Op.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectTypeBindM(expected = Type.Int64, actual = tpe, bind = tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop'.", loc)
      }

      case KindedAst.Expr.Binary(sop, exp1, exp2, tvar, loc) => sop match {

        case SemanticOp.BoolOp.And | SemanticOp.BoolOp.Or =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
          c.expectTypeM(expected = Type.Bool, actual = tpe2, exp2.loc)
          c.unifyTypeM(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Float32Op.Add | SemanticOp.Float32Op.Sub | SemanticOp.Float32Op.Mul | SemanticOp.Float32Op.Div
             | SemanticOp.Float32Op.Exp =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectTypeM(expected = Type.Float32, actual = tpe1, exp1.loc)
          c.expectTypeM(expected = Type.Float32, actual = tpe2, exp2.loc)
          c.unifyTypeM(tvar, Type.Float32, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Float64Op.Add | SemanticOp.Float64Op.Sub | SemanticOp.Float64Op.Mul | SemanticOp.Float64Op.Div
             | SemanticOp.Float64Op.Exp =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectTypeM(expected = Type.Float64, actual = tpe1, exp1.loc)
          c.expectTypeM(expected = Type.Float64, actual = tpe2, exp2.loc)
          c.unifyTypeM(tvar, Type.Float64, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int8Op.Add | SemanticOp.Int8Op.Sub | SemanticOp.Int8Op.Mul | SemanticOp.Int8Op.Div
             | SemanticOp.Int8Op.Rem | SemanticOp.Int8Op.Exp
             | SemanticOp.Int8Op.And | SemanticOp.Int8Op.Or | SemanticOp.Int8Op.Xor =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectTypeM(expected = Type.Int8, actual = tpe1, exp1.loc)
          c.expectTypeM(expected = Type.Int8, actual = tpe2, exp2.loc)
          c.unifyTypeM(tvar, Type.Int8, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int16Op.Add | SemanticOp.Int16Op.Sub | SemanticOp.Int16Op.Mul | SemanticOp.Int16Op.Div
             | SemanticOp.Int16Op.Rem | SemanticOp.Int16Op.Exp
             | SemanticOp.Int16Op.And | SemanticOp.Int16Op.Or | SemanticOp.Int16Op.Xor =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectTypeM(expected = Type.Int16, actual = tpe1, exp1.loc)
          c.expectTypeM(expected = Type.Int16, actual = tpe2, exp2.loc)
          c.unifyTypeM(tvar, Type.Int16, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int32Op.Add | SemanticOp.Int32Op.Sub | SemanticOp.Int32Op.Mul | SemanticOp.Int32Op.Div
             | SemanticOp.Int32Op.Rem | SemanticOp.Int32Op.Exp
             | SemanticOp.Int32Op.And | SemanticOp.Int32Op.Or | SemanticOp.Int32Op.Xor =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectTypeM(expected = Type.Int32, actual = tpe1, exp1.loc)
          c.expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          c.unifyTypeM(tvar, Type.Int32, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int64Op.Add | SemanticOp.Int64Op.Sub | SemanticOp.Int64Op.Mul | SemanticOp.Int64Op.Div
             | SemanticOp.Int64Op.Rem | SemanticOp.Int64Op.Exp
             | SemanticOp.Int64Op.And | SemanticOp.Int64Op.Or | SemanticOp.Int64Op.Xor =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectTypeM(expected = Type.Int64, actual = tpe1, exp1.loc)
          c.expectTypeM(expected = Type.Int64, actual = tpe2, exp2.loc)
          c.unifyTypeM(tvar, Type.Int64, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int8Op.Shl | SemanticOp.Int8Op.Shr
             | SemanticOp.Int16Op.Shl | SemanticOp.Int16Op.Shr
             | SemanticOp.Int32Op.Shl | SemanticOp.Int32Op.Shr
             | SemanticOp.Int64Op.Shl | SemanticOp.Int64Op.Shr =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.unifyTypeM(tvar, tpe1, loc)
          c.expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.BoolOp.Eq | SemanticOp.BoolOp.Neq
             | SemanticOp.CharOp.Eq | SemanticOp.CharOp.Neq
             | SemanticOp.Float32Op.Eq | SemanticOp.Float32Op.Neq
             | SemanticOp.Float64Op.Eq | SemanticOp.Float64Op.Neq
             | SemanticOp.Int8Op.Eq | SemanticOp.Int8Op.Neq
             | SemanticOp.Int16Op.Eq | SemanticOp.Int16Op.Neq
             | SemanticOp.Int32Op.Eq | SemanticOp.Int32Op.Neq
             | SemanticOp.Int64Op.Eq | SemanticOp.Int64Op.Neq =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.unifyTypeM(tpe1, tpe2, loc)
          c.unifyTypeM(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.CharOp.Lt | SemanticOp.CharOp.Le | SemanticOp.CharOp.Gt | SemanticOp.CharOp.Ge
             | SemanticOp.Float32Op.Lt | SemanticOp.Float32Op.Le | SemanticOp.Float32Op.Gt | SemanticOp.Float32Op.Ge
             | SemanticOp.Float64Op.Lt | SemanticOp.Float64Op.Le | SemanticOp.Float64Op.Gt | SemanticOp.Float64Op.Ge
             | SemanticOp.Int8Op.Lt | SemanticOp.Int8Op.Le | SemanticOp.Int8Op.Gt | SemanticOp.Int8Op.Ge
             | SemanticOp.Int16Op.Lt | SemanticOp.Int16Op.Le | SemanticOp.Int16Op.Gt | SemanticOp.Int16Op.Ge
             | SemanticOp.Int32Op.Lt | SemanticOp.Int32Op.Le | SemanticOp.Int32Op.Gt | SemanticOp.Int32Op.Ge
             | SemanticOp.Int64Op.Lt | SemanticOp.Int64Op.Le | SemanticOp.Int64Op.Gt | SemanticOp.Int64Op.Ge =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.unifyTypeM(tpe1, tpe2, loc)
          c.unifyTypeM(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.StringOp.Concat =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectTypeM(expected = Type.Str, actual = tpe1, exp1.loc)
          c.expectTypeM(expected = Type.Str, actual = tpe2, exp2.loc)
          c.unifyTypeM(tvar, Type.Str, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case _ => throw InternalCompilerException(s"Unexpected binary operator: '$sop'.", loc)
      }

      case Expr.IfThenElse(exp1, exp2, exp3, loc) =>
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        val (tpe3, eff3) = visitExp(exp3)
        c.expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
        c.unifyTypeM(tpe2, tpe3, loc)
        val resTpe = tpe3
        val resEff = Type.mkUnion(eff1, eff2, eff3, loc)
        (resTpe, resEff)

      case Expr.Stm(exp1, exp2, loc) =>
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        val resTpe = tpe2
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)

      case Expr.Discard(exp, loc) =>
        val (tpe, eff) = visitExp(exp)
        val resTpe = Type.Unit
        val resEff = eff
        (resTpe, resEff)

      case Expr.Let(sym, mod, exp1, exp2, loc) =>
        val (tpe1, eff1) = visitExp(exp1)
        c.unifyTypeM(sym.tvar, tpe1, loc)
        val (tpe2, eff2) = visitExp(exp2)
        val resTpe = tpe2
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)

      case Expr.LetRec(sym, ann, mod, exp1, exp2, loc) =>
        // Ensure that `exp1` is a lambda.
        //val a = Type.freshVar(Kind.Star, loc)
        //val b = Type.freshVar(Kind.Star, loc)
        //val p = Type.freshVar(Kind.Eff, loc)
        //val expectedType = Type.mkArrowWithEffect(a, p, b, loc)
        val (tpe1, eff1) = visitExp(exp1)
        //c.unifyTypeM(expectedType, tpe1, exp1.loc)
        c.unifyTypeM(sym.tvar, tpe1, exp1.loc)
        val (tpe2, eff2) = visitExp(exp2)
        val resTpe = tpe2
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)

      case Expr.Region(tpe, loc) =>
        val resTpe = tpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Scope(sym, regionVar, exp, evar, loc) =>
        c.enterRegionM(regionVar.sym)
        c.unifyTypeM(sym.tvar, Type.mkRegion(regionVar, loc), loc)
        val (tpe, eff) = visitExp(exp)
        c.exitRegionM(evar, eff, loc)
        val resTpe = tpe
        val resEff = evar
        (resTpe, resEff)

      case Expr.ScopeExit(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionTpe = Type.mkRegion(regionVar, loc)
        val evar = Type.freshVar(Kind.Eff, loc)
        val (tpe1, _) = visitExp(exp1)
        val (tpe2, _) = visitExp(exp2)
        c.expectTypeM(expected = Type.mkArrowWithEffect(Type.Unit, evar, Type.Unit, loc.asSynthetic), actual = tpe1, exp1.loc)
        c.expectTypeM(expected = regionTpe, actual = tpe2, exp2.loc)
        val resTpe = Type.Unit
        val resEff = Type.mkUnion(Type.IO, regionVar, loc)
        (resTpe, resEff)

      case Expr.Match(exp, rules, loc) =>
        val patterns = rules.map(_.pat)
        val guards = rules.flatMap(_.guard)
        val bodies = rules.map(_.exp)
        val guardLocs = guards.map(_.loc)
        val (tpe, eff) = visitExp(exp)
        val patternTypes = patterns.map(visitPattern)
        c.unifyAllTypesM(tpe :: patternTypes, Kind.Star, loc)
        val (guardTpes, guardEffs) = guards.map(visitExp).unzip
        guardTpes.zip(guardLocs).foreach { case (gTpe, gLoc) => c.expectTypeM(expected = Type.Bool, actual = gTpe, loc = gLoc) }
        val (bodyTypes, bodyEffs) = bodies.map(visitExp).unzip
        val resTpe = c.unifyAllTypesM(bodyTypes, Kind.Star, loc)
        val resEff = Type.mkUnion(eff :: guardEffs ::: bodyEffs, loc)
        (resTpe, resEff)

      case Expr.TypeMatch(exp, rules, loc) =>
        val bodies = rules.map(_.exp)
        val (tpe, eff) = visitExp(exp)
        // rigidify all the type vars in the rules
        rules.flatMap(_.tpe.typeVars.toList).map(_.sym).foreach(c.rigidifyM)
        // unify each rule's variable with its type
        rules.foreach { rule => c.unifyTypeM(rule.sym.tvar, rule.tpe, rule.sym.loc) }
        val (bodyTypes, bodyEffs) = bodies.map(visitExp).unzip
        val resTpe = c.unifyAllTypesM(bodyTypes, Kind.Star, loc)
        val resEff = Type.mkUnion(eff :: bodyEffs, loc)
        (resTpe, resEff)

      case e@Expr.RestrictableChoose(star, exp, rules, tpe, loc) => RestrictableChooseConstraintGeneration.visitRestrictableChoose(e)

      case KindedAst.Expr.Tag(symUse, exp, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = root.enums(symUse.sym.enumSym)

        // Lookup the case declaration.
        val caze = decl.cases(symUse.sym)

        // Instantiate the type scheme of the case.
        val (_, tagType) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        //
        // The tag type is a function from the type of variant to the type of the enum.
        //
        val (tpe, eff) = visitExp(exp)
        c.unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)

      case e@Expr.RestrictableTag(sym, exp, isOpen, tpe, loc) => RestrictableChooseConstraintGeneration.visitRestrictableTag(e)

      case Expr.Tuple(elms, loc) =>
        val (elmTpes, elmEffs) = elms.map(visitExp).unzip
        val resTpe = Type.mkTuple(elmTpes, loc)
        val resEff = Type.mkUnion(elmEffs, loc)
        (resTpe, resEff)

      case Expr.RecordEmpty(loc) =>
        val resTpe = Type.mkRecord(Type.RecordRowEmpty, loc)
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.RecordSelect(exp, label, tvar, loc) =>
        //
        // r : { label = tpe | row }
        // -------------------------
        //       r.label : tpe
        //
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        val expectedRowType = Type.mkRecordRowExtend(label, tvar, freshRowVar, loc)
        val expectedRecordType = Type.mkRecord(expectedRowType, loc)
        val (tpe, eff) = visitExp(exp)
        c.unifyTypeM(tpe, expectedRecordType, loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)

      case Expr.RecordExtend(label, exp1, exp2, tvar, loc) =>
        //
        //       exp1 : tpe        exp2 : {| r }
        // ---------------------------------------------
        // { label = exp1 | exp2 } : { label  :: tpe | r }
        //
        val restRow = Type.freshVar(Kind.RecordRow, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.unifyTypeM(tpe2, Type.mkRecord(restRow, loc), loc)
        c.unifyTypeM(tvar, Type.mkRecord(Type.mkRecordRowExtend(label, tpe1, restRow, loc), loc), loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)

      case Expr.RecordRestrict(label, exp, tvar, loc) =>
        //
        //  exp : { label  :: t | r }
        // -------------------------
        // { -label | exp } : {| r }
        //
        val freshLabelType = Type.freshVar(Kind.Star, loc)
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        val (tpe, eff) = visitExp(exp)
        c.unifyTypeM(tpe, Type.mkRecord(Type.mkRecordRowExtend(label, freshLabelType, freshRowVar, loc), loc), loc)
        c.unifyTypeM(tvar, Type.mkRecord(freshRowVar, loc), loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)

      case Expr.ArrayLit(exps, exp, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (tpes, effs) = exps.map(visitExp).unzip
        val (tpe, eff) = visitExp(exp)
        c.expectTypeM(expected = regionType, actual = tpe, exp.loc)
        val elmTpe = c.unifyAllTypesM(tpes, Kind.Star, loc)
        c.unifyTypeM(tvar, Type.mkArray(elmTpe, regionVar, loc), loc)
        c.unifyTypeM(evar, Type.mkUnion(Type.mkUnion(effs, loc), eff, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.ArrayNew(exp1, exp2, exp3, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        val (tpe3, eff3) = visitExp(exp3)
        c.expectTypeM(expected = regionType, actual = tpe1, loc)
        c.expectTypeM(expected = Type.Int32, actual = tpe3, exp3.loc)
        c.unifyTypeM(tvar, Type.mkArray(tpe2, regionVar, loc), loc)
        c.unifyTypeM(evar, Type.mkUnion(eff1, eff2, eff3, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.ArrayLoad(exp1, exp2, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectTypeM(expected = Type.mkArray(tvar, regionVar, loc), actual = tpe1, exp1.loc)
        c.expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
        c.unifyTypeM(evar, Type.mkUnion(regionVar, eff1, eff2, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.ArrayStore(exp1, exp2, exp3, evar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val arrayType = Type.mkArray(elmVar, regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        val (tpe3, eff3) = visitExp(exp3)
        c.expectTypeM(expected = arrayType, actual = tpe1, exp1.loc)
        c.expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
        c.expectTypeM(expected = elmVar, actual = tpe3, exp3.loc)
        c.unifyTypeM(evar, Type.mkUnion(List(regionVar, eff1, eff2, eff3), loc), loc)
        val resTpe = Type.Unit
        val resEff = evar
        (resTpe, resEff)

      case Expr.ArrayLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val (tpe, eff) = visitExp(exp)
        c.expectTypeM(Type.mkArray(elmVar, regionVar, loc), tpe, exp.loc)
        //        unbindVar(elmVar)
        //        unbindVar(regionVar)
        // TODO ASSOC-TYPES is there an unbind equivalent?
        val resTpe = Type.Int32
        val resEff = eff
        (resTpe, resEff)

      case Expr.VectorLit(exps, tvar, evar, loc) =>
        val (tpes, effs) = exps.map(visitExp).unzip
        val tpe = c.unifyAllTypesM(tpes, Kind.Star, loc)
        c.unifyTypeM(tvar, Type.mkVector(tpe, loc), loc)
        c.unifyTypeM(evar, Type.mkUnion(effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.VectorLoad(exp1, exp2, tvar, evar, loc) =>
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectTypeM(expected = Type.mkVector(tvar, loc), actual = tpe1, exp1.loc)
        c.expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
        c.unifyTypeM(evar, Type.mkUnion(eff1, eff2, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.VectorLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val (tpe, eff) = visitExp(exp)
        c.expectTypeM(Type.mkVector(elmVar, loc), tpe, exp.loc)
        //        _ <- unbindVar(elmVar) // TODO ASSOC-TYPES
        val resTpe = Type.Int32
        val resEff = eff
        (resTpe, resEff)

      case Expr.Ref(exp1, exp2, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectTypeM(tpe2, regionType, exp2.loc)
        c.unifyTypeM(tvar, Type.mkRef(tpe1, regionVar, loc), loc)
        c.unifyTypeM(evar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.Deref(exp, tvar, evar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val refType = Type.mkRef(elmVar, regionVar, loc)
        val (tpe, eff) = visitExp(exp)
        c.expectTypeM(expected = refType, actual = tpe, exp.loc)
        c.unifyTypeM(tvar, elmVar, loc)
        c.unifyTypeM(evar, Type.mkUnion(eff, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.Assign(exp1, exp2, evar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val refType = Type.mkRef(elmVar, regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectTypeM(expected = refType, actual = tpe1, exp1.loc)
        c.expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
        c.unifyTypeM(evar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
        val resTpe = Type.Unit
        val resEff = evar
        (resTpe, resEff)

      case Expr.Ascribe(exp, expectedTpe, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        val (actualTpe, actualEff) = visitExp(exp)
        c.expectTypeM(expected = expectedTpe.getOrElse(Type.freshVar(Kind.Star, loc)), actual = actualTpe, loc)
        c.unifyTypeM(actualTpe, tvar, loc)
        c.expectTypeM(expected = expectedEff.getOrElse(Type.freshVar(Kind.Eff, loc)), actual = actualEff, loc)
        val resTpe = tvar
        val resEff = actualEff
        (resTpe, resEff)

      case Expr.InstanceOf(exp, clazz, loc) =>
        val (tpe, eff) = visitExp(exp)
        c.expectTypeM(expected = Type.Pure, actual = eff, exp.loc)
        val resTpe = Type.Bool
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.CheckedCast(cast, exp, tvar, evar, loc) =>
        cast match {
          case Ast.CheckedCastType.TypeCast =>
            // Ignore the inferred type of exp.
            val (_, eff) = visitExp(exp)
            c.unifyTypeM(evar, eff, loc)
            val resTpe = tvar
            val resEff = evar
            (resTpe, resEff)

          case Ast.CheckedCastType.EffectCast =>
            // We simply union the purity and effect with a fresh variable.
            val (tpe, eff) = visitExp(exp)
            c.unifyTypeM(tvar, tpe, loc)
            val resTpe = tvar
            val resEff = Type.mkUnion(eff, evar, loc)
            (resTpe, resEff)
        }

      case Expr.UncheckedCast(exp, declaredTpe, declaredEff, tvar, loc) =>
        // A cast expression is unsound; the type system assumes the declared type is correct.
        val (actualTyp, actualEff) = visitExp(exp)
        c.unifyTypeM(tvar, declaredTpe.getOrElse(actualTyp), loc)
        val resTpe = tvar
        val resEff = declaredEff.getOrElse(actualEff)
        (resTpe, resEff)

      case Expr.UncheckedMaskingCast(exp, loc) =>
        val (tpe, eff) = visitExp(exp)
        val resTpe = tpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Without(exp, effUse, loc) =>
        val effType = Type.Cst(TypeConstructor.Effect(effUse.sym), effUse.loc)
        //        val expected = Type.mkDifference(Type.freshVar(Kind.Bool, loc), effType, loc)
        // TODO EFF-MIGRATION use expected
        val (tpe, eff) = visitExp(exp)
        val resTpe = tpe
        val resEff = eff
        (resTpe, resEff)

      case Expr.TryCatch(exp, rules, loc) =>
        val (tpes, effs) = rules.map {
          case KindedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }.unzip
        val (tpe, eff) = visitExp(exp)
        val ruleTpe = c.unifyAllTypesM(tpes, Kind.Star, loc)
        c.unifyTypeM(tpe, ruleTpe, loc)
        val resTpe = tpe
        val resEff = Type.mkUnion(eff :: effs, loc)
        (resTpe, resEff)

      case Expr.TryWith(exp, effUse, rules, tvar, loc) =>
        val effect = root.effects(effUse.sym)
        val ops = effect.ops.map(op => op.sym -> op).toMap
        val continuationEffect = Type.freshVar(Kind.Eff, loc)

        def unifyFormalParams(op: Symbol.OpSym, expected: List[KindedAst.FormalParam], actual: List[KindedAst.FormalParam]): Unit = {
          if (expected.length != actual.length) {
            () // MATT mismatched arity error
          } else {
            c.expectTypeArguments(op, expectedTypes = expected.map(_.tpe), actualTypes = actual.map(_.tpe), actual.map(_.loc), loc)
          }
        }

        def visitHandlerRule(rule: KindedAst.HandlerRule, tryBlockTpe: Type): (Type, Type) = rule match {
          case KindedAst.HandlerRule(op, actualFparams0, body, opTvar) =>
            // Don't need to generalize since ops are monomorphic
            // Don't need to handle unknown op because resolver would have caught this
            val (actualFparams, List(resumptionFparam)) = actualFparams0.splitAt(actualFparams0.length - 1)
            ops(op.sym) match {
              case KindedAst.Op(_, KindedAst.Spec(_, _, _, _, expectedFparams, _, opTpe, _, _, _, _)) =>
                val resumptionArgType = opTpe
                val resumptionResType = tryBlockTpe
                val resumptionEff = continuationEffect
                val expectedResumptionType = Type.mkArrowWithEffect(resumptionArgType, resumptionEff, resumptionResType, loc.asSynthetic)
                unifyFormalParams(op.sym, expected = expectedFparams, actual = actualFparams)
                c.expectTypeM(expected = expectedResumptionType, actual = resumptionFparam.tpe, resumptionFparam.loc)
                val (actualTpe, actualEff) = visitExp(body)

                // unify the operation return type with its tvar
                c.unifyTypeM(actualTpe, opTvar, body.loc)

                // unify the handler result type with the whole block's tvar
                c.unifyTypeM(tvar, actualTpe, body.loc)
                (actualTpe, actualEff)
            }
        }

        // MATT need to do something regiony here
        val (tpe, bodyEff) = visitExp(exp)
        val correctedBodyEff = c.purifyEff(effUse.sym, bodyEff) // Note: Does not work for polymorphic effects.
        val (_, effs) = rules.map(visitHandlerRule(_, tpe)).unzip
        c.unifyTypeM(continuationEffect, Type.mkUnion(correctedBodyEff :: effs, loc), loc)
        c.unifyTypeM(tvar, tpe, loc)
        val resultTpe = tpe
        val resultEff = continuationEffect
        (resultTpe, resultEff)

      case Expr.Do(op, args, tvar, loc) =>
        val effect = root.effects(op.sym.eff)
        val operation = effect.ops.find(_.sym == op.sym)
          .getOrElse(throw InternalCompilerException(s"Unexpected missing operation $op in effect ${op.sym.eff}", loc))
        val effTpe = Type.Cst(TypeConstructor.Effect(op.sym.eff), loc)

        def visitArg(arg: KindedAst.Expr, fparam: KindedAst.FormalParam): Type = {
          val (tpe, eff) = visitExp(arg)
          c.expectTypeM(expected = fparam.tpe, actual = tpe, arg.loc)
          eff
        }

        val effs = (args zip operation.spec.fparams) map {
          case (arg, fparam) => visitArg(arg, fparam)
        }

        c.unifyTypeM(operation.spec.tpe, tvar, loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(effTpe :: operation.spec.eff :: effs, loc)

        (resTpe, resEff)

      case Expr.InvokeConstructor(constructor, args, loc)
      =>
        val classTpe = getFlixType(constructor.getDeclaringClass)
        val (_, _) = args.map(visitExp).unzip
        val resTpe = classTpe
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.InvokeMethod(method, clazz, exp, args, loc)
      =>
        val classTpe = getFlixType(clazz)
        val (baseTyp, _) = visitExp(exp)
        c.unifyTypeM(baseTyp, classTpe, loc)
        val (_, _) = args.map(visitExp).unzip
        val resTpe = getFlixType(method.getReturnType)
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.InvokeStaticMethod(method, args, loc)
      =>
        val returnTpe = getFlixType(method.getReturnType)
        val (_, _) = args.map(visitExp).unzip
        val resTpe = getFlixType(method.getReturnType)
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.GetField(field, clazz, exp, loc)
      =>
        val classType = getFlixType(clazz)
        val fieldType = getFlixType(field.getType)
        val (tpe, _) = visitExp(exp)
        c.expectTypeM(expected = classType, actual = tpe, exp.loc)
        val resTpe = fieldType
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.PutField(field, clazz, exp1, exp2, loc)
      =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(clazz)
        val (tpe1, _) = visitExp(exp1)
        val (tpe2, _) = visitExp(exp2)
        c.expectTypeM(expected = classType, actual = tpe1, exp1.loc)
        c.expectTypeM(expected = fieldType, actual = tpe2, exp2.loc)
        val resTpe = Type.Unit
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.GetStaticField(field, loc)
      =>
        val fieldType = getFlixType(field.getType)
        val resTpe = fieldType
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.PutStaticField(field, exp, loc)
      =>
        val (valueTyp, _) = visitExp(exp)
        c.expectTypeM(expected = getFlixType(field.getType), actual = valueTyp, exp.loc)
        val resTpe = Type.Unit
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.NewObject(name, clazz, methods, loc)
      =>

        /**
          * Generates constraints for the JVM method.
          */
        def visitJvmMethod(method: KindedAst.JvmMethod): Unit = method match {
          case KindedAst.JvmMethod(ident, fparams, exp, returnTpe, eff, loc) =>

            /**
              * Constrains the given formal parameter to its declared type.
              */
            def visitFormalParam(fparam: KindedAst.FormalParam): Unit = fparam match {
              case KindedAst.FormalParam(sym, _, tpe, _, loc) =>
                c.unifyTypeM(sym.tvar, tpe, loc)
            }

            fparams.foreach(visitFormalParam)
            val (bodyTpe, bodyEff) = visitExp(exp)
            c.expectTypeM(expected = returnTpe, actual = bodyTpe, exp.loc)
          // TODO ASSOC-TYPES check eff matches declared eff ?
        }

        methods.foreach(visitJvmMethod)
        val resTpe = getFlixType(clazz)
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.NewChannel(exp1, exp2, tvar, loc)
      =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectTypeM(expected = regionType, actual = tpe1, exp1.loc)
        c.expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(eff1, eff2, regionVar, loc)
        (resTpe, resEff)

      case Expr.GetChannel(exp, tvar, loc)
      =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val elmVar = Type.freshVar(Kind.Star, loc)
        val channelType = Type.mkReceiver(elmVar, regionVar, loc)
        val (tpe, eff) = visitExp(exp)
        c.expectTypeM(expected = channelType, actual = tpe, exp.loc)
        c.unifyTypeM(tvar, elmVar, loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(eff, regionVar, loc)
        (resTpe, resEff)

      case Expr.PutChannel(exp1, exp2, loc)
      =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val elmVar = Type.freshVar(Kind.Star, loc)
        val channelType = Type.mkSender(elmVar, regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectTypeM(expected = channelType, actual = tpe1, exp1.loc)
        c.expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
        val resTpe = Type.mkUnit(loc)
        val resEff = Type.mkUnion(eff1, eff2, regionVar, loc)
        (resTpe, resEff)

      case Expr.SelectChannel(rules, default, tvar, loc)
      =>

        val regionVar = Type.freshVar(Kind.Eff, loc)

        /**
          * Generates constraints for the SelectChannelRule.
          */
        def visitSelectRule(sr0: KindedAst.SelectChannelRule): (Type, Type) = {
          sr0 match {
            case KindedAst.SelectChannelRule(sym, chan, body) =>
              val (chanType, eff1) = visitExp(chan)
              val (bodyType, eff2) = visitExp(body)
              c.unifyTypeM(chanType, Type.mkReceiver(sym.tvar, regionVar, sym.loc), sym.loc)
              val resTpe = bodyType
              val resEff = Type.mkUnion(eff1, eff2, regionVar, loc)
              (resTpe, resEff)
          }
        }

        /**
          * Generates constraints for the default rule.
          */
        def visitDefaultRule(exp0: Option[KindedAst.Expr]): (Type, Type) =
          exp0 match {
            case None => (Type.freshVar(Kind.Star, loc), Type.Pure)
            case Some(exp) => visitExp(exp)
          }

        val (ruleTypes, ruleEffs) = rules.map(visitSelectRule).unzip
        val (defaultType, eff2) = visitDefaultRule(default)
        c.unifyAllTypesM(tvar :: defaultType :: ruleTypes, Kind.Star, loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(regionVar :: eff2 :: ruleEffs, loc)
        (resTpe, resEff)

      case Expr.Spawn(exp1, exp2, loc)
      =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (tpe1, _) = visitExp(exp1)
        val (tpe2, _) = visitExp(exp2)
        c.expectTypeM(expected = regionType, actual = tpe2, exp2.loc)
        val resTpe = Type.Unit
        val resEff = Type.mkUnion(Type.IO, regionVar, loc)
        (resTpe, resEff)

      case Expr.ParYield(frags, exp, loc)
      =>
        val patterns = frags.map(_.pat)
        val parExps = frags.map(_.exp)
        val patLocs = frags.map(_.loc)
        val (tpe, eff) = visitExp(exp)
        val patternTypes = patterns.map(visitPattern)
        val (fragTypes, fragEffs) = parExps.map(visitExp).unzip
        patternTypes.zip(fragTypes).zip(patLocs).foreach { case ((patTpe, expTpe), l) => c.unifyTypeM(patTpe, expTpe, l) }
        fragEffs.zip(patLocs).foreach { case (p, l) => c.expectTypeM(expected = Type.Pure, actual = p, l) }
        val resTpe = tpe
        val resEff = eff
        (resTpe, resEff)

      case Expr.Lazy(exp, loc)
      =>
        val (tpe, eff) = visitExp(exp)
        c.expectTypeM(expected = Type.Pure, actual = eff, exp.loc)
        val resTpe = Type.mkLazy(tpe, loc)
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Force(exp, tvar, loc)
      =>
        val (tpe, eff) = visitExp(exp)
        c.expectTypeM(expected = Type.mkLazy(tvar, loc), actual = tpe, exp.loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)

      case e
        @Expr.FixpointConstraintSet(cs, tvar, loc) => SchemaConstraintGeneration.visitFixpointConstraintSet(e)
      case e
        @Expr.FixpointLambda(pparams, exp, tvar, loc) => SchemaConstraintGeneration.visitFixpointLambda(e)
      case e
        @Expr.FixpointMerge(exp1, exp2, loc) => SchemaConstraintGeneration.visitFixpointMerge(e)
      case e
        @Expr.FixpointSolve(exp, loc) => SchemaConstraintGeneration.visitFixpointSolve(e)
      case e
        @Expr.FixpointFilter(pred, exp, tvar, loc) => SchemaConstraintGeneration.visitFixpointFilter(e)
      case e
        @Expr.FixpointInject(exp, pred, tvar, loc) => SchemaConstraintGeneration.visitFixpointInject(e)
      case e
        @Expr.FixpointProject(pred, exp1, exp2, tvar, loc) => SchemaConstraintGeneration.visitFixpointProject(e)
      case Expr.Error(m, tvar, evar)
      =>
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)
    }
  }

  /**
    * Generates constraints for the pattern.
    */
  def visitPattern(pat0: KindedAst.Pattern)(implicit c: TypingContext, root: KindedAst.Root, flix: Flix): Type = {
    // Make the context's level available
    // This is a def rather than a val because c is mutable.
    implicit def level: Level = c.level

    pat0 match {

      case KindedAst.Pattern.Wild(tvar, loc) => tvar

      case KindedAst.Pattern.Var(sym, tvar, loc) =>
        c.unifyTypeM(sym.tvar, tvar, loc)
        tvar

      case KindedAst.Pattern.Cst(cst, loc) => TypeReconstruction.constantType(cst)

      case KindedAst.Pattern.Tag(symUse, pat, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = root.enums(symUse.sym.enumSym)

        // Lookup the case declaration.
        val caze = decl.cases(symUse.sym)

        // Instantiate the type scheme of the case.
        val (_, tagType) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        //
        // The tag type is a function from the type of variant to the type of the enum.
        //
        val tpe = visitPattern(pat)
        c.unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
        tvar


      case KindedAst.Pattern.Tuple(elms, loc) =>
        val tpes = elms.map(visitPattern)
        Type.mkTuple(tpes, loc)

      case KindedAst.Pattern.Record(pats, pat, tvar, loc) =>
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc.asSynthetic)
        val freshRecord = Type.mkRecord(freshRowVar, loc.asSynthetic)

        def mkRecordType(patTypes: List[(Name.Label, Type, SourceLocation)]): Type = {
          val ps = patTypes.foldRight(freshRowVar: Type) {
            case ((lbl, t, l), acc) => Type.mkRecordRowExtend(
              lbl, t, acc, l)
          }
          Type.mkRecord(ps, loc)
        }

        val tailTpe = visitPattern(pat)
        c.unifyTypeM(freshRecord, tailTpe, loc.asSynthetic)
        val patTpes = pats.map(visitRecordLabelPattern)
        val resTpe = mkRecordType(patTpes)
        c.unifyTypeM(resTpe, tvar, loc)
        resTpe

      case KindedAst.Pattern.RecordEmpty(loc) => Type.mkRecord(Type.RecordRowEmpty, loc)

      case KindedAst.Pattern.Error(tvar, _) => tvar

    }
  }

  /**
    * Generates constraints for the record label pattern.
    */
  private def visitRecordLabelPattern(pat: KindedAst.Pattern.Record.RecordLabelPattern)(implicit c: TypingContext, root: KindedAst.Root, flix: Flix): (Name.Label, Type, SourceLocation) = pat match {
    case KindedAst.Pattern.Record.RecordLabelPattern(label, tvar, p, loc) =>
      // { Label = Pattern ... }
      val tpe = visitPattern(p)
      c.unifyTypeM(tpe, tvar, loc)
      (label, tpe, loc)
  }

}
