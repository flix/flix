/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.ast.shared

trait QualifiedSym{
  def namespace: List[String]
  def name: String
}
