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
package ca.uwaterloo.flix.api.lsp.provider

/**
  * CompletionPriority
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
object CompletionPriority {
  def high(name: String): String = "1" + name

  def boost(name: String): String = "2" + name

  def snippet(name: String): String = "4" + name

  def local(name: String): String = "5" + name

  def normal(name: String): String = "7" + name

  def low(name: String): String = "9" + name
}
