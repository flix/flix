/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.TypedAst

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

object PackageTrust {

  def run(root: TypedAst.Root)(implicit flix: Flix): List[SuspiciousExpr] = {
    implicit val sctx: SharedContext = SharedContext.mk()
    sctx.exprs.asScala.toList
  }


  /** Companion object for [[SharedContext]] */
  private object SharedContext {

    /** Returns a fresh shared context. */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A globally shared context. Must be thread-safe.
    *
    * @param exprs the [[SuspiciousExpr]]s in the AST, if any.
    */
  private case class SharedContext(exprs: ConcurrentLinkedQueue[SuspiciousExpr])


}
