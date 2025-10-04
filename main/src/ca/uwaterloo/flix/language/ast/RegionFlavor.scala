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
sealed trait RegionFlavor extends Ordered[RegionFlavor] {
  override def compare(that: RegionFlavor): Int = {
    def ordinal(f: RegionFlavor): Int = f match {
      case RegionFlavor.Lofi => 0
      case RegionFlavor.Hifi => 1
      case RegionFlavor.XHifi => 2
    }

    ordinal(this) - ordinal(that)
  }
}

// MATT docs
object RegionFlavor {
  case object Lofi extends RegionFlavor
  case object Hifi extends RegionFlavor
  case object XHifi extends RegionFlavor
}
