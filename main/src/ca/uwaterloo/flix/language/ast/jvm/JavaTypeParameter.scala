/*
 * Copyright 2026 Flix Authors
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
package ca.uwaterloo.flix.language.ast.jvm

/**
  * A declared Java type parameter and its upper bounds.
  *
  * Java type parameters can declare only upper bounds. Lower bounds occur on wildcard type arguments and are
  * represented by [[JavaType.Wildcard]].
  */
case class JavaTypeParameter(variable: JavaTypeVariable, upperBounds: List[JavaType])
