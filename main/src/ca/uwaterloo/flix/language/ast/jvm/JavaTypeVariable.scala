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
  * Identifies a Java type variable by its name and declaring class or method.
  *
  * Type-variable names are scoped to their declaration: unrelated declarations can each define `T`, and a method can
  * shadow a class variable named `T`. The owner is therefore part of the identity so these variables remain distinct.
  */
case class JavaTypeVariable(owner: JavaTypeVariableOwner, name: String)
