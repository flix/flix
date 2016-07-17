/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.verifier

import com.microsoft.z3.Model

/**
  * A common super-type that represents the result of an SMT query.
  */
sealed trait SmtResult

object SmtResult {

  /**
    * The SMT query is satisfiable, i.e. it has at least one model.
    *
    * @param model a model that satisfies the SMT query.
    */
  case class Satisfiable(model: Model) extends SmtResult

  /**
    * The SMT query is unsatisfiable, i.e. it has no model.
    */
  case object Unsatisfiable extends SmtResult

  /**
    * The SMT query may or may not be satisfiable, i.e. it is unknown if there is a model.
    */
  case object Unknown extends SmtResult

}

