/*
 * Copyright 2024 Holger Dal Mogensen
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
package ca.uwaterloo.flix.language.ast.shared


/**
  * Companion object of [[Annotations]].
  */
object Annotations {
  /**
    * The empty sequence of annotations.
    */
  val Empty: Annotations = Annotations(Nil)
}

/**
  * A sequence of annotations.
  */
case class Annotations(annotations: List[Annotation]) {

  /**
    * Returns `true` if `this` sequence contains the `@Deprecated` annotation.
    */
  def isDeprecated: Boolean = annotations exists (_.isInstanceOf[Annotation.Deprecated])

  /**
    * Returns `true` if `this` sequence contains the `@Experimental` annotation.
    */
  def isExperimental: Boolean = annotations exists (_.isInstanceOf[Annotation.Experimental])

  /**
    * Returns `true` if `this` sequence contains the `@Export` annotation.
    */
  def isExport: Boolean = annotations exists (_.isInstanceOf[Annotation.Export])

  /**
    * Returns `true` if `this` sequence contains the `@Internal` annotation.
    */
  def isInternal: Boolean = annotations exists (_.isInstanceOf[Annotation.Internal])

  /**
    * Returns `true` if `this` sequence contains the `@Lazy` annotation.
    */
  def isLazy: Boolean = annotations exists (_.isInstanceOf[Annotation.Lazy])

  /**
    * Returns `true` if `this` sequence contains the `@LazyWhenPure` annotation.
    */
  def isLazyWhenPure: Boolean = annotations exists (_.isInstanceOf[Annotation.LazyWhenPure])

  /**
    * Returns `true` if `this` sequence contains the `@MustUse` annotation.
    */
  def isMustUse: Boolean = annotations exists (_.isInstanceOf[Annotation.MustUse])

  /**
    * Returns `true` if `this` sequence contains the `@Parallel` annotation.
    */
  def isParallel: Boolean = annotations exists (_.isInstanceOf[Annotation.Parallel])

  /**
    * Returns `true` if `this` sequence contains the `@ParallelWhenPure` annotation.
    */
  def isParallelWhenPure: Boolean = annotations exists (_.isInstanceOf[Annotation.ParallelWhenPure])

  /**
    * Returns `true` if `this` sequence contains the `@Skip` annotation.
    */
  def isSkip: Boolean = annotations exists (_.isInstanceOf[Annotation.Skip])

  /**
    * Returns `true` if `this` sequence contains the `@Test` annotation.
    */
  def isTest: Boolean = annotations exists (_.isInstanceOf[Annotation.Test])
}
