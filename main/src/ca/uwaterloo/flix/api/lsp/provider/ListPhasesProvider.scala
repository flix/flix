/*
 * Copyright 2022 Jonathan Lindegaard Starup
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.language.phase._

object ListPhasesProvider {
  val phases: List[String] = List(
    classOf[Parser].getSimpleName, // Parser is different because its a class
    simpleName(Weeder.getClass),
    simpleName(Namer.getClass),
    simpleName(Resolver.getClass),
    simpleName(Kinder.getClass),
    simpleName(Deriver.getClass),
    simpleName(Typer.getClass),
    simpleName(EntryPoint.getClass),
    simpleName(Statistics.getClass),
    simpleName(Stratifier.getClass),
    simpleName(Regions.getClass),
    simpleName(Redundancy.getClass),
    simpleName(Safety.getClass),
    simpleName(Documentor.getClass),
    simpleName(Lowering.getClass),
    simpleName(EarlyTreeShaker.getClass),
    simpleName(Monomorph.getClass),
    simpleName(Simplifier.getClass),
    simpleName(ClosureConv.getClass),
    simpleName(LambdaLift.getClass),
    simpleName(Tailrec.getClass),
    simpleName(Optimizer.getClass),
    simpleName(LateTreeShaker.getClass),
    simpleName(VarNumbering.getClass),
    simpleName(Finalize.getClass),
    simpleName(Eraser.getClass)
  )

  /**
    * Returns the source name of a scala object (without the internally
    * suffixed `$`.
    */
  private def simpleName(obj: Class[_]): String = {
    obj.getSimpleName.dropRight(1)
  }
}
