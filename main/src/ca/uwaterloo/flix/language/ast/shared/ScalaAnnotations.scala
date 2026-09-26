/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

object ScalaAnnotations {
  /**
    * Represents that the annotated element is introduced by the class `clazz`.
    */
  case class IntroducedBy(clazz: java.lang.Class[?]) extends scala.annotation.StaticAnnotation

  /**
    * Represents that the annotated element is eliminated by the class `clazz`.
    */
  case class EliminatedBy(clazz: java.lang.Class[?]) extends scala.annotation.StaticAnnotation
}
