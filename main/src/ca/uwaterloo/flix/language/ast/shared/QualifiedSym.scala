/*
 * Copyright 2025 Chenhao Gao
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

trait QualifiedSym {
  def qnamespace: List[String]
  def qname: String
  def depth: Int = qnamespace.length
  override def toString: String = qnamespace.mkString(".") + "." + qname

  /**
   * Checks to see if `this` is ambiguous with respect to the given `SymbolSet`.
    */
  private def isAmbiguous(wrt : SymbolSet): Boolean = {
    wrt.enums.exists(x => x.qname == qname && x.qnamespace != qnamespace) ||
    wrt.structs.exists(x => x.qname == qname && x.qnamespace != qnamespace) ||
    wrt.traits.exists(x => x.qname == qname && x.qnamespace != qnamespace) ||
    wrt.effects.exists(x => x.qname == qname && x.qnamespace != qnamespace)
  }

  /**
    * Formats the qualified name of this symbol, ensuring that it is distinct
    *
    * @param wrt The set of symbols which it must not be ambiguous with respect to.
    */
  def formatDistinct(wrt : SymbolSet): String = {
    if (isAmbiguous(wrt)) (qnamespace.mkString(".") + "." + qname)
    else qname
  }
}
