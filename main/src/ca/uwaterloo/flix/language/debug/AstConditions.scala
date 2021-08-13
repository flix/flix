package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Ast.IntroducedBy
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.language.phase.Kinder

import scala.reflect.runtime.universe

object AstConditions {

  private val IntroducedByType = universe.typeOf[IntroducedBy].typeConstructor
  private val TestAnnType = universe.typeOf[TestAnn[_]].typeConstructor
//  private val EliminatedBySymbol = universe.typeOf[EliminatedBy].typeSymbol.asType

  def checkAstAfterPhases(ast: Product, phases: List[Phase[_, _]]): Unit = {

  }

  def visitNode(node0: Any, path: List[String], phases: List[universe.Type]): List[FailedCondition] = node0 match {
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
      val rest = product.productIterator.zip(product.productElementNames).flatMap {
        case (node, name) => visitNode(node, name :: path, phases)
      }.toList
      failedConditions ++ rest
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
//      case EliminatedBySymbol =>
      case TestAnnType => Some(Condition.EliminatedBy(getConditionAnnotationType(ann)))
      case _ => None
    }
  }

  private def getConditionAnnotationType(ann: universe.Annotation): universe.Type= {
    //    println("asdf")
    //    println(ann)
    val typeArgs = ann.tree.tpe.typeArgs
    println(ann.tree.tpe.typeConstructor)
    val stringType = typeForClass(classOf[String])
    val argType = typeArgs.head
    println(stringType =:= argType)
    println("debug")
    //    println(ann.tree.tpe.getClass)
    //    println(ann.tree.children.tail.head.asInstanceOf[Class[_]])
    //    ann.tree.children.tail.head.symbol.asClass
    throw new Exception("")
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
}
