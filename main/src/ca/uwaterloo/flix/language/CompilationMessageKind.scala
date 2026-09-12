/*
 * Copyright 2024 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.CompilationMessageKind.*

sealed trait CompilationMessageKind {
  override def toString: String = this match {
    case DefaultHandlerError => "Default Handler Error"
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
    case TerminationError => "Termination Error"
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
    case TypeError => Some(DefaultHandlerError)
    case DefaultHandlerError => Some(EntryPointError)
    case EntryPointError => Some(InstanceError)
    case InstanceError => Some(StratificationError)
    case StratificationError => Some(PatternMatchError)
    case PatternMatchError => Some(RedundancyError)
    case RedundancyError => Some(SafetyError)
    case SafetyError => Some(TerminationError)
    case TerminationError => None
    case TestError => None
  }
}

object CompilationMessageKind {

  case object DefaultHandlerError extends CompilationMessageKind

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

  case object TerminationError extends CompilationMessageKind

  case object TestError extends CompilationMessageKind

  case object TypeError extends CompilationMessageKind

  case object WeederError extends CompilationMessageKind

}
