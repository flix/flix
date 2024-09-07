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
  * Using a priority, we can make certain suggestions occur earlier in
  * the list of suggestions by generating a SortText using `Priority.toSortText`
  * on the relevant priority and label. This SortText can then be used in the `CompletionItem`
  * for the suggestion.
  *
  * In practice, we leverage the alphabetical sorting of completions by
  * associating each priority with a number. The higher the priority,
  * the lower the number. Then `Priority.toSortText`
  * prepends that number to the label. This results in completions which
  * are firstly sorted by the priority and then by name.
  *
  */
sealed trait Priority {
}
object Priority {
  case object Highest extends Priority
  case object Higher extends Priority
  case object High extends Priority
  case object Default extends Priority
  case object Low extends Priority
  case object Lower extends Priority
  case object Lowest extends Priority

  /**
    * Returns a sortText string which comes earlier alphabetically the higher the priority. 
    *
    * The outputs of this, when sorted alphabetically, prefers inputs with 
    * higher priorities. The higher the priority, the earlier in an alphabetical
    * sorting it will occur.
    *
    * @param p      the priority to be used in the sortText.
    * @param label  the label to be used in the sortText.
    * @return a string which is listed earlier alphabetically the higher the priority.
    */
  def toSortText(p: Priority, label: String): String = p match {
  	case Priority.Highest => s"1$label"
  	case Priority.Higher  => s"2$label"
  	case Priority.High    => s"3$label"
  	case Priority.Default => s"4$label"
  	case Priority.Low     => s"5$label"
  	case Priority.Lower   => s"6$label"
  	case Priority.Lowest  => s"7$label"
  }
}
