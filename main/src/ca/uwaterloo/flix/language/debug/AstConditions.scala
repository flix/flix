package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Ast.{EliminatedBy, IntroducedBy}
import ca.uwaterloo.flix.language.phase.{Kinder, Phase}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.reflect.runtime.universe

object AstConditions {

  private val IntroducedByType = universe.typeOf[IntroducedBy[_]].typeConstructor
  private val EliminatedBySymbol = universe.typeOf[EliminatedBy[_]].typeConstructor

  def checkAstAfterPhases(ast: Product, phases: List[Phase[_, _]]): Unit = {
    val failures = visitNode(ast, ast.productPrefix :: Nil, phases.map(phase => typeForClass(phase.getClass)))
    failures match {
      case Nil => ()
      case failure :: _ => throw InternalCompilerException(s"AST Condition failure: \n${formatFailedCondition(failure)}")
    }
  }

  private def visitNode(node0: Any, path: List[String], phases: List[universe.Type]): List[FailedCondition] = node0 match {
    case iterable: Iterable[_] =>
      iterable.zipWithIndex.flatMap {
        case (node, index) => visitNode(node, index.toString :: path, phases)
      }.toList
    case product: Product =>
      // Check whether there are any conditions on this node type
      val conditions = getClassAnnotations(product).flatMap(processAnnotation)
      val failedConditions = conditions.flatMap {
        case cond@Condition.IntroducedBy(phase) if !phases.exists(p => p =:= phase) => Some(FailedCondition(product, path, cond))
        case cond@Condition.EliminatedBy(phase) if phases.exists(p => p =:= phase) => Some(FailedCondition(product, path, cond))
        case _ => None
      }
      // Recurse on the nested nodes
      val rest = product.productIterator.zip(product.productElementNames).flatMap {
        case (node, name) => visitNode(node, name :: path, phases)
      }.toList
      failedConditions ++ rest
    case _ => Nil
  }

  case class TestAnn[T <: Phase[_, _]]() extends scala.annotation.StaticAnnotation

  @TestAnn[Kinder.type]
  object TestClass
  def main(args: Array[String]): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val thing = TestClass
    val mirror = ru.runtimeMirror(thing.getClass.getClassLoader)
    val reflection = mirror.reflect(thing)

    println(processAnnotation(reflection.symbol.annotations.head))
  }

  private def getClassAnnotations(obj: Any): List[universe.Annotation] = {
    val mirror = universe.runtimeMirror(obj.getClass.getClassLoader)
    val reflection = mirror.reflect(obj)
    reflection.symbol.annotations
  }


  private def processAnnotation(ann: universe.Annotation): Option[Condition] = {
    ann.tree.tpe.typeConstructor match {
      case IntroducedByType => Some(Condition.IntroducedBy(getConditionAnnotationType(ann)))
      case EliminatedBySymbol => Some(Condition.EliminatedBy(getConditionAnnotationType(ann)))
      case _ => None
    }
  }

  private def getConditionAnnotationType(ann: universe.Annotation): universe.Type = {
    ann.tree.tpe.typeArgs.head
  }

  private def typeForClass(clazz: Class[_]): universe.Type = {
    universe.runtimeMirror(clazz.getClassLoader).classSymbol(clazz).toType
  }

  private sealed trait Condition
  private object Condition {
    case class IntroducedBy(phase: universe.Type) extends Condition
    case class EliminatedBy(phase: universe.Type) extends Condition
  }

  private case class FailedCondition(obj: Any, path: List[String], cond: Condition)

  private def formatFailedCondition(cond: FailedCondition): String = cond match {
    case FailedCondition(obj, path, Condition.IntroducedBy(phase)) =>
      s"${obj.getClass.getName} should have been introduced by phase $phase, but was present at ${path.reverse.mkString(".")}"
    case FailedCondition(obj, path, Condition.EliminatedBy(phase)) =>
      s"${obj.getClass.getName} should have been eliminated by phase $phase, but was present at ${path.reverse.mkString(".")}"
  }
}
