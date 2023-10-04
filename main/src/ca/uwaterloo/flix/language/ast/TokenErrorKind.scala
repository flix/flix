/*
 * Copyright 2023 Herluf Baggesen
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

sealed trait TokenErrorKind

object TokenErrorKind {

  case object BlockCommentTooDeep extends TokenErrorKind

  case object DoubleDottedNumber extends TokenErrorKind

  case object UnexpectedChar extends TokenErrorKind

  case object UnexpectedCharWithinBuiltIn extends TokenErrorKind

  case object UnexpectedCharWithinChar extends TokenErrorKind

  case object UnexpectedCharWithinInfixFunction extends TokenErrorKind

  case object UnterminatedBlockComment extends TokenErrorKind

  case object UnterminatedBuiltIn extends TokenErrorKind

  case object UnterminatedChar extends TokenErrorKind

  case object UnterminatedInfixFunction extends TokenErrorKind

  case object UnterminatedString extends TokenErrorKind

  case object UnterminatedStringInterpolation extends TokenErrorKind

  case object StringInterpolationTooDeep extends TokenErrorKind
}
