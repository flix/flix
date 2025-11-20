/*
 * Copyright 2024 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.CompilationMessageKind.*

sealed trait CompilationMessageKind {
  override def toString: String = this match {
    case DerivationError => "Derivation Error"
    case EntryPointError => "Entry Point Error"
    case InstanceError => "Instance Error"
    case KindError => "Kind Error"
    case LexerError => "Lexer Error"
    case NameError => "Name Error"
    case ParseError => "Parse Error"
    case PatternMatchError => "Pattern Match Error"
    case RedundancyError => "Redundancy Error"
    case ResolutionError => "Resolution Error"
    case SafetyError => "Safety Error"
    case StratificationError => "Stratification Error"
    case TestError => "Test Error"
    case TypeError => "Type Error"
    case WeederError => "Syntax Error"
  }

  /**
    * Returns the next compilation message kind in the phase order.
    *
    * Returns `None` if this is the last phase.
    */
  def next: Option[CompilationMessageKind] = this match {
    case LexerError => Some(ParseError)
    case ParseError => Some(WeederError)
    case WeederError => Some(NameError)
    case NameError => Some(ResolutionError)
    case ResolutionError => Some(KindError)
    case KindError => Some(DerivationError)
    case DerivationError => Some(TypeError)
    case TypeError => Some(EntryPointError)
    case EntryPointError => Some(InstanceError)
    case InstanceError => Some(StratificationError)
    case StratificationError => Some(PatternMatchError)
    case PatternMatchError => Some(RedundancyError)
    case RedundancyError => Some(SafetyError)
    case SafetyError => None
    case TestError => None
  }
}

object CompilationMessageKind {

  case object DerivationError extends CompilationMessageKind

  case object EntryPointError extends CompilationMessageKind

  case object InstanceError extends CompilationMessageKind

  case object KindError extends CompilationMessageKind

  case object LexerError extends CompilationMessageKind

  case object NameError extends CompilationMessageKind

  case object ParseError extends CompilationMessageKind

  case object PatternMatchError extends CompilationMessageKind

  case object RedundancyError extends CompilationMessageKind

  case object ResolutionError extends CompilationMessageKind

  case object SafetyError extends CompilationMessageKind

  case object StratificationError extends CompilationMessageKind

  case object TestError extends CompilationMessageKind

  case object TypeError extends CompilationMessageKind

  case object WeederError extends CompilationMessageKind

}
