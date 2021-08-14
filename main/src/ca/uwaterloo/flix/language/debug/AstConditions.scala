/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Ast.{EliminatedBy, IntroducedBy}
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.reflect.runtime.universe

/**
  * Verifies that annotations describing conditions on the AST are fulfilled.
  */
object AstConditions {

  /**
    * The type constructor of the [[IntroducedBy]] annotation.
    */
  private val IntroducedByType = universe.typeOf[IntroducedBy[_]].typeConstructor

  /**
    * The type constructor of the [[EliminatedBy]] annotation.
    */
  private val EliminatedBySymbol = universe.typeOf[EliminatedBy[_]].typeConstructor

  /**
    * Checks that conditions hold on the given `ast`, after having performed the given `phases`.
    */
  def checkAstAfterPhases(ast: Product, phases: List[Phase[_, _]]): Unit = {
    val failures = visitNode(ast, ast.productPrefix :: Nil, phases.map(phase => typeForClass(phase.getClass)))
    failures match {
      case Nil => ()
      case failure :: _ => throw InternalCompilerException(s"AST Condition failure after phase ${phases.last}: \n${formatFailedCondition(failure)}")
    }
  }

  /**
    * Checks conditions on the given `node`, with the given (reversed) path from root.
    */
  private def visitNode(node0: Any, path: List[String], phases: List[universe.Type]): List[FailedCondition] = node0 match {
    case iterable: Iterable[_] =>
      iterable.zipWithIndex.flatMap {
        case (node, index) => visitNode(node, index.toString :: path, phases)
      }.toList
    case product: Product =>
      // Check whether there are any conditions on this node type
      val conditions = getClassAnnotations(product).flatMap(processAnnotation)
      val failedConditions = findFailures(conditions, product, path, phases)
      // Recurse on the nested nodes
      val rest = product.productIterator.zip(product.productElementNames).flatMap {
        case (node, name) => visitNode(node, name :: path, phases)
      }.toList
      failedConditions ++ rest
    case obj =>
      // Check whether there are any conditions on this node type
      val conditions = getClassAnnotations(obj).flatMap(processAnnotation)
      findFailures(conditions, obj, path, phases)
  }

  private def findFailures(conditions: List[Condition], obj: Any, path: List[String], phases: List[universe.Type]): List[FailedCondition] = conditions.flatMap {
    case cond@Condition.IntroducedBy(phase) if !phases.exists(p => p =:= phase) => Some(FailedCondition(obj, path, cond))
    case cond@Condition.EliminatedBy(phase) if phases.exists(p => p =:= phase) => Some(FailedCondition(obj, path, cond))
    case _ => None
  }

  /**
    * Gets the annotations on the class of the given object.
    */
  private def getClassAnnotations(obj: Any): List[universe.Annotation] = {
    val mirror = universe.runtimeMirror(obj.getClass.getClassLoader)
    val reflection = mirror.reflect(obj)
    reflection.symbol.annotations
  }

  /**
    * Returns a Condition corresponding to the given annotation.
    */
  private def processAnnotation(ann: universe.Annotation): Option[Condition] = {
    ann.tree.tpe.typeConstructor match {
      case IntroducedByType => Some(Condition.IntroducedBy(getConditionAnnotationType(ann)))
      case EliminatedBySymbol => Some(Condition.EliminatedBy(getConditionAnnotationType(ann)))
      case _ => None
    }
  }

  /**
    * Gets the type argument of the given condition annotation.
    * For example, the argument of `@EliminatedBy[Typer]` is `Typer`.
    */
  private def getConditionAnnotationType(ann: universe.Annotation): universe.Type = {
    ann.tree.tpe.typeArgs.head
  }

  /**
    * Gets the type for the given class.
    */
  private def typeForClass(clazz: Class[_]): universe.Type = {
    universe.runtimeMirror(clazz.getClassLoader).classSymbol(clazz).toType
  }

  /**
    * A common super type for AST conditions.
    */
  private sealed trait Condition

  private object Condition {
    /**
      * An AST condition indicating that the annotated class should be introduced by the given `phase`.
      */
    case class IntroducedBy(phase: universe.Type) extends Condition

    /**
      * An AST condition indicating that the annotated class should be eliminated by the given `phase`.
      */
    case class EliminatedBy(phase: universe.Type) extends Condition
  }

  /**
    * Represents that the given `obj` located at `path` fails the condition `cond`.
    */
  private case class FailedCondition(obj: Any, path: List[String], cond: Condition)

  /**
    * Formats the failed condition for printing to the console.
    */
  private def formatFailedCondition(cond: FailedCondition): String = cond match {
    case FailedCondition(obj, path, Condition.IntroducedBy(phase)) =>
      s"${obj.getClass.getName} should have been introduced by phase $phase, but was present at ${path.reverse.mkString(".")}"
    case FailedCondition(obj, path, Condition.EliminatedBy(phase)) =>
      s"${obj.getClass.getName} should have been eliminated by phase $phase, but was present at ${path.reverse.mkString(".")}"
  }
}
