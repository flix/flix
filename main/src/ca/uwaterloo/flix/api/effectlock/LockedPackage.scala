/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.util.Sha256

/**
  * The signatures that one package is locked at.
  *
  * A signature is recorded by the hash of the scheme it was declared with, see [[HashType]], and
  * is named by the symbol it belongs to, e.g. `Museum.Clerk.sell`.
  *
  * The defs and the sigs of a package are kept apart, so that a def and a sig may share a name
  * without one of them standing for the other.
  *
  * @param defs the hash of each public def of the package.
  * @param sigs the hash of each public sig of the package.
  */
case class LockedPackage(defs: Map[String, Sha256], sigs: Map[String, Sha256])
