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
import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext

sealed trait CompilationMessageKind extends Ordered[CompilationMessageKind] {
  override def toString: String = this match {
    case DerivationError => "Derivation Error"
    case EntryPointError => "Entry Point Error"
    case InstanceError => "Instance Error"
    case KindError => "Kind Error"
    case LexerError => "Lexer Error"
    case NameError => "Name Error"
    case ParseError(sctx) => s"Parse Error ($sctx)"
    case PatternMatchError => "Pattern Match Error"
    case RedundancyError => "Redundancy Error"
    case ResolutionError => "Resolution Error"
    case SafetyError => "Safety Error"
    case StratificationError => "Stratification Error"
    case TestError => "Test Error"
    case TypeError => "Type Error"
    case WeederError => "Syntax Error"
  }

  override def compare(that: CompilationMessageKind): Int = (this, that) match {
    case _ if this == that => 0
    case (DerivationError, _) => -1
    case (EntryPointError, DerivationError) => 1
    case (EntryPointError, _) => -1
    case (InstanceError, DerivationError) => 1
    case (InstanceError, EntryPointError) => 1
    case (InstanceError, _) => -1
    case (KindError, DerivationError) => 1
    case (KindError, EntryPointError) => 1
    case (KindError, InstanceError) => 1
    case (KindError, _) => -1
    case (LexerError, DerivationError) => 1
    case (LexerError, EntryPointError) => 1
    case (LexerError, InstanceError) => 1
    case (LexerError, KindError) => 1
    case (LexerError, _) => -1
    case (NameError, DerivationError) => 1
    case (NameError, EntryPointError) => 1
    case (NameError, InstanceError) => 1
    case (NameError, KindError) => 1
    case (NameError, LexerError) => 1
    case (NameError, _) => -1
    case (ParseError(_), DerivationError) => 1
    case (ParseError(_), EntryPointError) => 1
    case (ParseError(_), InstanceError) => 1
    case (ParseError(_), KindError) => 1
    case (ParseError(_), LexerError) => 1
    case (ParseError(_), NameError) => 1
    case (ParseError(_), ParseError(_)) => 0
    case (ParseError(_), _) => -1
    case (PatternMatchError, DerivationError) => 1
    case (PatternMatchError, EntryPointError) => 1
    case (PatternMatchError, InstanceError) => 1
    case (PatternMatchError, KindError) => 1
    case (PatternMatchError, LexerError) => 1
    case (PatternMatchError, NameError) => 1
    case (PatternMatchError, ParseError(_)) => 1
    case (PatternMatchError, _) => -1
    case (RedundancyError, _) => 1
    case (RedundancyError, ResolutionError) => -1
    case (RedundancyError, SafetyError) => -1
    case (RedundancyError, StratificationError) => -1
    case (RedundancyError, TestError) => -1
    case (RedundancyError, TypeError) => -1
    case (RedundancyError, WeederError) => -1
    case (ResolutionError, _) => 1
    case (ResolutionError, SafetyError) => -1
    case (ResolutionError, StratificationError) => -1
    case (ResolutionError, TestError) => -1
    case (ResolutionError, TypeError) => -1
    case (ResolutionError, WeederError) => -1
    case (SafetyError, _) => 1
    case (SafetyError, StratificationError) => -1
    case (SafetyError, TestError) => -1
    case (SafetyError, TypeError) => -1
    case (SafetyError, WeederError) => -1
    case (StratificationError, _) => 1
    case (StratificationError, TestError) => -1
    case (StratificationError, TypeError) => -1
    case (StratificationError, WeederError) => -1
    case (TestError, _) => 1
    case (TestError, TypeError) => -1
    case (TestError, WeederError) => -1
    case (TypeError, _) => 1
    case (TypeError, WeederError) => -1
    case (WeederError, _) => 1
  }
}

object CompilationMessageKind {

  case object DerivationError extends CompilationMessageKind

  case object EntryPointError extends CompilationMessageKind

  case object InstanceError extends CompilationMessageKind

  case object KindError extends CompilationMessageKind

  case object LexerError extends CompilationMessageKind

  case object NameError extends CompilationMessageKind

  case class ParseError(sctx: SyntacticContext) extends CompilationMessageKind

  case object PatternMatchError extends CompilationMessageKind

  case object RedundancyError extends CompilationMessageKind

  case object ResolutionError extends CompilationMessageKind

  case object SafetyError extends CompilationMessageKind

  case object StratificationError extends CompilationMessageKind

  case object TestError extends CompilationMessageKind

  case object TypeError extends CompilationMessageKind

  case object WeederError extends CompilationMessageKind

}
