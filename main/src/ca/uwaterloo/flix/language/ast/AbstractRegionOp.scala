/*
 * Copyright 2025 Matthew Lutze
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
package ca.uwaterloo.flix.language.ast

// MATT docs
sealed trait AbstractRegionOp extends Ordered[AbstractRegionOp] {
  override def compare(that: AbstractRegionOp): Int = {
    def ordinal(op: AbstractRegionOp): Int = op match {
      case AbstractRegionOp.GetAlloc => 0
      case AbstractRegionOp.GetRead => 1
      case AbstractRegionOp.GetWrite => 2
      case AbstractRegionOp.XWrite => 3
    }

    ordinal(this) - ordinal(that)
  }
}

// MATT docs
object AbstractRegionOp {
  case object GetAlloc extends AbstractRegionOp

  case object GetRead extends AbstractRegionOp

  case object GetWrite extends AbstractRegionOp

  case object XWrite extends AbstractRegionOp
}
