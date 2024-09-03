/*
 * Copyright 2022 Paul Butcher, Lukas Rønn, Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.api.lsp.provider.completion

/**
  * Priority for completions.
  * 
  * 
  *
  * To ensure that completions are displayed "most useful" first, we precede sortText with a number. Priorities
  * differ depending on the type of completion, and can be boosted depending upon context (e.g. type completions
  * are boosted if the cursor is preceded by a ":")
  *
  * 1: High: completions which are only available within a very specific context
  * 2: Boost: completions which are normally low priority, but the context makes them more likely
  * 4: Snippet: snippets are relatively high priority because they're rare, and to be useful at all they need to be available
  * 5: Local: local variables
  * 7: Normal: completions that are relevant within no particular context
  * 9: Low: completions that are unlikely to be relevant unless within a specific context
  */
sealed trait Priority {
}
object Priority {
  case object highest extends Priority
  case object higher extends Priority
  case object high extends Priority
  case object low extends Priority
  case object lower extends Priority
  case object lowest extends Priority

  def toSortText(label: String, p: Priority): String = p match {
  	case Priority.highest => 1 + label
  	case Priority.higher  => 2 + label
  	case Priority.high    => 3 + label
  	case Priority.low     => 4 + label
  	case Priority.lower   => 5 + label
  	case Priority.lowest  => 6 + label
  }
}
