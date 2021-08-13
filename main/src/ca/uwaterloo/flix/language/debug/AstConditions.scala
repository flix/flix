package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Ast.IntroducedBy
import ca.uwaterloo.flix.language.phase.Phase

import scala.reflect.runtime.{currentMirror, universe}

object AstConditions {

  private val IntroducedBySymbol = universe.typeOf[IntroducedBy].typeSymbol.asType
  private val TestAnnSymbol = universe.typeOf[TestAnn].typeSymbol.asType
//  private val EliminatedBySymbol = universe.typeOf[EliminatedBy].typeSymbol.asType

  def checkAstAfterPhases(ast: Product, phases: List[Phase[_, _]]): Unit = {

  }

  def visitNode(node0: Any, path: List[String], phases: List[Phase[_, _]]): List[(Any, List[String])] = node0 match {
    case iterable: Iterable[_] =>
      iterable.zipWithIndex.flatMap {
        case (node, index) => visitNode(node, index.toString :: path, phases)
      }.toList
//    case product: Product => product.getClass.getAnnotation(Ast.IntroducedBy.getClass)
  }

  case class TestAnn(arg: Class[_]) extends scala.annotation.StaticAnnotation

  @TestAnn(classOf[String])
  object TestClass
  def main(args: Array[String]): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val thing = TestClass
    val mirror = ru.runtimeMirror(thing.getClass.getClassLoader)
    val reflection = mirror.reflect(thing)

    val testAnn = universe.typeOf[TestAnn].typeSymbol.asType
    println(reflection.symbol.annotations.head.tree)
    println(reflection.symbol.annotations)
    println(processAnnotation(reflection.symbol.annotations.head))
    println(reflection.symbol.annotations.head.tree.tpe)
    println(reflection.symbol.annotations.head.tree.children.tail)
  }

  private def getClassAnnotations(obj: Any): List[universe.Annotation] = {
    val mirror = universe.runtimeMirror(obj.getClass.getClassLoader)
    val reflection = mirror.reflect(obj)
    reflection.symbol.annotations
  }


  private def processAnnotation(ann: universe.Annotation): Option[Condition] = {
    ann.tree.tpe.typeSymbol match {
      case IntroducedBySymbol => Some(Condition.IntroducedBy(getConditionAnnotationClass(ann)))
//      case EliminatedBySymbol =>
      case TestAnnSymbol => Some(Condition.EliminatedBy(getConditionAnnotationClass(ann)))
      case _ => None
    }
  }

  private def getConditionAnnotationClass(ann: universe.Annotation): universe.ClassSymbol = {
    println(ann.tree.children.tail.head.asInstanceOf[Class[_]])
    ann.tree.children.tail.head.symbol.asClass
  }

  private def symbolForClass(clazz: Class[_]): universe.ClassSymbol = {
    universe.runtimeMirror(clazz.getClassLoader).classSymbol(clazz)
  }

  private sealed trait Condition
  private object Condition {
    case class IntroducedBy(clazz: universe.ClassSymbol) extends Condition
    case class EliminatedBy(clazz: universe.ClassSymbol) extends Condition
  }
}
