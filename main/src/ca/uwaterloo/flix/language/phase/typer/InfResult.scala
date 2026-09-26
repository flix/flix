/*
 * Copyright 2024 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.ast.{RigidityEnv, Type}

/**
  *
  * A result of performing type inference.
  *
  * @param constrs constraints inferred for the expression
  * @param tpe     the inferred type of the expression
  * @param eff     the inferred effect of the expression
  * @param renv    the inferred rigidity environment for the expression (marking region variables)
  */
case class InfResult(constrs: List[TypeConstraint], tpe: Type, eff: Type, renv: RigidityEnv)
