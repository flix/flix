/*
 * Copyright 2025 Flix Contributors
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, ParYieldFragment, Pattern, Root}
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.PatMatchError
import ca.uwaterloo.flix.language.fmt.FormatConstant
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * Pattern Match Exhaustiveness and Redundancy Checker.
  *
  * Checks that every `match` expression is exhaustive (covers all possible values)
  * and that no pattern is redundant (unreachable because earlier patterns already
  * cover all values it could match).
  *
  * Implements the usefulness predicate from:
  * Maranget, L. (2007). Warnings for pattern matching.
  * Journal of Functional Programming, 17(3), 387-421.
  *
  * The core idea: a pattern vector `q` is **useful** w.r.t. a pattern matrix `P`
  * if there exists a value matched by `q` but not by any row of `P`.
  *
  * - **Exhaustiveness**: the wildcard vector `(_, ..., _)` is useful w.r.t.
  *   the matrix of all unguarded rules => the match is non-exhaustive.
  * - **Redundancy**: rule `i` is redundant if its pattern is NOT useful w.r.t.
  *   the matrix of all preceding unguarded rules.
  */
object PatMatch2 {

  // ─────────────────────────────────────────────────────────────
  //  Witness Pattern ADT — used only for synthesizing error messages
  // ─────────────────────────────────────────────────────────────

  /**
    * A synthetic pattern representing a value not matched by any rule.
    * Used only for constructing human-readable error messages in
    * [[PatMatchError.NonExhaustiveMatch]]. Unlike `TypedAst.Pattern`, witness
    * patterns carry no `Type` or `SourceLocation`.
    */
  private sealed trait WitnessPattern

  private object WitnessPattern {
    /** Matches any value. Printed as `_`. */
    case object Wildcard extends WitnessPattern

    /** A literal constant, e.g. `true`, `'a'`, `42`. */
    case class Literal(cst: Constant) extends WitnessPattern

    /** An enum case, e.g. `Some(_)`, `None`. */
    case class Tag(sym: Symbol.CaseSym, args: List[WitnessPattern]) extends WitnessPattern

    /** A tuple, e.g. `(_, _)`. */
    case class Tuple(elms: List[WitnessPattern]) extends WitnessPattern

    /** A record, e.g. `{ x = _ | _ }`. */
    case class Record(labels: List[(Name.Label, WitnessPattern)], tail: WitnessPattern) extends WitnessPattern
  }

  // ─────────────────────────────────────────────────────────────
  //  Pattern Matrix
  // ─────────────────────────────────────────────────────────────

  /**
    * A matrix of patterns with a known column count (`width`).
    *
    * Each row is a `List[Pattern]` of length `width`. Bundling rows
    * and width together eliminates bugs where they get out of sync.
    *
    * Example (width = 2):
    * {{{
    *   [ [Some(_), true ],
    *     [None,    _    ] ]
    * }}}
    */
  private case class PatternMatrix(rows: List[List[Pattern]], width: Int) {
    /** Returns true if the matrix has no rows. */
    def isEmpty: Boolean = rows.isEmpty
  }

  // ─────────────────────────────────────────────────────────────
  //  Pattern Classification Helpers
  // ─────────────────────────────────────────────────────────────

  /**
    * Returns `true` if `pat` is a wildcard-like pattern: `Wild`, `Var`, or `Error`.
    *
    * These patterns match any value and carry no sub-patterns.
    *
    * Example:
    * {{{
    *   isWildcard(Pattern.Wild(...))       = true
    *   isWildcard(Pattern.Var(...))        = true
    *   isWildcard(Pattern.Cst(42, ...))    = false
    *   isWildcard(Pattern.Tag(Some, ...))  = false
    * }}}
    */
  private def isWildcard(pat: Pattern): Boolean = pat match {
    case _: Pattern.Wild  => true
    case _: Pattern.Var   => true
    case _: Pattern.Error => true
    case _                => false
  }

  /**
    * Returns the number of sub-patterns (arity) of a constructor pattern.
    *
    * - Wildcard-like patterns: 0 (they are not constructors)
    * - `Cst`: 0 (constants have no sub-patterns)
    * - `Tag(sym, pats)`: `pats.length`
    * - `Tuple(pats)`: `pats.length`
    * - `Record(pats, tail)`: `pats.length + 1` (labels + tail)
    *
    * Example:
    * {{{
    *   patternArity(Pattern.Cst(42, ...))                = 0
    *   patternArity(Pattern.Tag(Some, [_], ...))          = 1
    *   patternArity(Pattern.Tuple([_, _], ...))           = 2
    *   patternArity(Pattern.Record([x, y], tail, ...))    = 3
    * }}}
    */
  private def patternArity(pat: Pattern): Int = pat match {
    case _: Pattern.Wild                => 0
    case _: Pattern.Var                 => 0
    case _: Pattern.Error               => 0
    case _: Pattern.Cst                 => 0
    case Pattern.Tag(_, pats, _, _)     => pats.length
    case Pattern.Tuple(pats, _, _)      => pats.length
    case Pattern.Record(pats, _, _, _)  => pats.length + 1
  }

  /**
    * Returns `true` if two constructor patterns represent the same constructor.
    *
    * - Two `Tag` patterns are the same if they share the same `CaseSym`.
    * - Two `Tuple` patterns are always the same (single constructor).
    * - Two `Record` patterns are always the same (single constructor shape).
    * - Two `Cst` patterns are the same if the constants are equal.
    * - All other combinations are different.
    *
    * Wildcard-like patterns should not be passed to this function.
    *
    * Example:
    * {{{
    *   sameConstructor(Tag(Some, ...), Tag(Some, ...)) = true
    *   sameConstructor(Tag(Some, ...), Tag(None, ...)) = false
    *   sameConstructor(Cst(true), Cst(true))           = true
    *   sameConstructor(Cst(true), Cst(false))          = false
    * }}}
    */
  private def sameConstructor(p1: Pattern, p2: Pattern): Boolean = (p1, p2) match {
    case (Pattern.Tag(CaseSymUse(s1, _), _, _, _), Pattern.Tag(CaseSymUse(s2, _), _, _, _)) => s1 == s2
    case (_: Pattern.Tuple, _: Pattern.Tuple)       => true
    case (_: Pattern.Record, _: Pattern.Record)     => true
    case (Pattern.Cst(c1, _, _), Pattern.Cst(c2, _, _)) => c1 == c2
    case _                                          => false
  }

  // ─────────────────────────────────────────────────────────────
  //  Matrix Operations
  // ─────────────────────────────────────────────────────────────

  /**
    * Computes the distinct head constructors (non-wildcard patterns) in the
    * first column of the matrix.
    *
    * This corresponds to Σ(P) in Maranget's algorithm. Duplicates (by
    * `sameConstructor`) are removed, keeping the first occurrence.
    *
    * Example: Given matrix (width=1):
    * {{{
    *   [ [Some(_)],
    *     [None],
    *     [_],
    *     [Some(1)] ]
    * }}}
    * Returns: `[Some(_), None]`
    */
  private def headConstructors(matrix: PatternMatrix): List[Pattern] = {
    matrix.rows.foldLeft(List.empty[Pattern]) { (acc, row) =>
      val head = row.head
      if (isWildcard(head)) acc
      else if (acc.exists(c => sameConstructor(c, head))) acc
      else acc :+ head
    }
  }

  /**
    * Specializes the pattern matrix for the given constructor pattern `ctor`.
    *
    * For each row in the matrix:
    * - If the head is a wildcard: expand it to `arity` wildcards + remaining columns.
    * - If the head is the same constructor as `ctor`: replace it with its sub-patterns + remaining columns.
    * - Otherwise: discard the row.
    *
    * The resulting matrix has width `arity + (width - 1)`.
    *
    * This corresponds to S(c, P) in Maranget's algorithm.
    *
    * Example: Given `ctor = Some(_)` (arity 1) and matrix (width=2):
    * {{{
    *   [ [Some(1), true ],
    *     [None,    false],
    *     [_,       true ] ]
    * }}}
    * Returns (width=2):
    * {{{
    *   [ [1,    true ],
    *     [_,    true ] ]
    * }}}
    */
  private def specialize(ctor: Pattern, matrix: PatternMatrix): PatternMatrix = {
    val arity = patternArity(ctor)
    val newWidth = arity + matrix.width - 1

    val newRows = matrix.rows.flatMap { row =>
      val head = row.head
      val tail = row.tail

      if (isWildcard(head)) {
        // Wildcard: expand to `arity` copies of the wildcard + remaining columns
        Some(List.fill(arity)(head) ::: tail)
      } else if (sameConstructor(ctor, head)) {
        // Same constructor: extract sub-patterns
        head match {
          case Pattern.Tag(_, pats, _, _)    => Some(pats ::: tail)
          case Pattern.Tuple(pats, _, _)     => Some(pats.toList ::: tail)
          case Pattern.Record(pats, pat, _, _) =>
            val fieldPats = pats.map(_.pat)
            Some(fieldPats ::: List(pat) ::: tail)
          case _: Pattern.Cst               => Some(tail)
          case _                            => None
        }
      } else {
        None
      }
    }

    PatternMatrix(newRows, newWidth)
  }

  /**
    * Specializes a single pattern vector `q` for the constructor `ctor`.
    *
    * Same logic as matrix specialization but for a single row.
    *
    * Returns `Some(expanded_row)` if the head of `q` matches `ctor` or is a wildcard,
    * or `None` if the head is a different constructor.
    */
  private def specializeVector(ctor: Pattern, q: List[Pattern]): Option[List[Pattern]] = {
    val arity = patternArity(ctor)
    val head = q.head
    val tail = q.tail

    if (isWildcard(head)) {
      Some(List.fill(arity)(head) ::: tail)
    } else if (sameConstructor(ctor, head)) {
      head match {
        case Pattern.Tag(_, pats, _, _)    => Some(pats ::: tail)
        case Pattern.Tuple(pats, _, _)     => Some(pats.toList ::: tail)
        case Pattern.Record(pats, pat, _, _) =>
          val fieldPats = pats.map(_.pat)
          Some(fieldPats ::: List(pat) ::: tail)
        case _: Pattern.Cst               => Some(tail)
        case _                            => None
      }
    } else {
      None
    }
  }

  /**
    * Computes the default matrix: rows whose first column is a wildcard,
    * with the first column removed.
    *
    * This corresponds to D(P) in Maranget's algorithm.
    *
    * Example: Given matrix (width=2):
    * {{{
    *   [ [Some(1), true ],
    *     [_,       false],
    *     [None,    true ] ]
    * }}}
    * Returns (width=1):
    * {{{
    *   [ [false] ]
    * }}}
    */
  private def computeDefaultMatrix(matrix: PatternMatrix): PatternMatrix = {
    val newRows = matrix.rows.flatMap { row =>
      if (isWildcard(row.head)) Some(row.tail)
      else None
    }
    PatternMatrix(newRows, matrix.width - 1)
  }

  /**
    * Determines whether the set of head constructors `sigma` forms a
    * complete signature for its type.
    *
    * | Type              | Complete when...                                |
    * |-------------------|-------------------------------------------------|
    * | Enum              | All cases of the enum appear in sigma            |
    * | Bool              | Both `true` and `false` appear                   |
    * | Unit              | `()` appears                                     |
    * | RecordEmpty       | `RecordEmpty` appears                            |
    * | Tuple             | Always complete (single constructor)             |
    * | Record            | Always complete (single constructor shape)        |
    * | Other (Char, Int) | Never complete (infinite type)                   |
    */
  private def isCompleteSignature(sigma: List[Pattern], root: Root): Boolean = {
    if (sigma.isEmpty) return false

    sigma.head match {
      // Enum: all cases must be present
      case Pattern.Tag(CaseSymUse(sym, _), _, _, _) =>
        val allCases = root.enums(sym.enumSym).cases.keySet
        allCases.forall(caseSym => sigma.exists {
          case Pattern.Tag(CaseSymUse(s, _), _, _, _) => s == caseSym
          case _ => false
        })

      // Tuple: always complete (single constructor)
      case _: Pattern.Tuple => true

      // Record: always complete (single constructor shape for a given label set)
      case _: Pattern.Record => true

      // Bool: need both true and false
      case Pattern.Cst(Constant.Bool(_), _, _) =>
        sigma.exists { case Pattern.Cst(Constant.Bool(true), _, _) => true; case _ => false } &&
        sigma.exists { case Pattern.Cst(Constant.Bool(false), _, _) => true; case _ => false }

      // Unit: just need Unit
      case Pattern.Cst(Constant.Unit, _, _) => true

      // RecordEmpty: just need RecordEmpty
      case Pattern.Cst(Constant.RecordEmpty, _, _) => true

      // Other constants (Char, Int*, Float*, BigInt, String): never complete
      case _: Pattern.Cst => false

      // Anything else: not complete
      case _ => false
    }
  }

  // ─────────────────────────────────────────────────────────────
  //  Core: Usefulness Predicate
  // ─────────────────────────────────────────────────────────────

  /**
    * Returns `true` if the pattern vector `q` is **useful** w.r.t. `matrix`.
    *
    * A vector is useful if there exists some value matched by `q` but not
    * matched by any row in the matrix. This is the core of Maranget's
    * algorithm (function U).
    *
    * - If `width = 0`: useful iff the matrix is empty (no rows to match).
    * - If `width > 0`:
    *   - Compute `sigma` = head constructors of the matrix.
    *   - If sigma is a complete signature: `q` is useful iff it is useful
    *     against the specialized matrix for at least one constructor.
    *   - If sigma is incomplete: check the default matrix, or if `q`'s
    *     head is a constructor, check the specialized matrix for that constructor.
    *
    * Example:
    * {{{
    *   matrix = [ [true, _] ]
    *   q      = [false, _]
    *   isUseful(matrix, q, root) = true  // false is not matched
    *
    *   matrix = [ [true, _], [false, _] ]
    *   q      = [_, _]
    *   isUseful(matrix, q, root) = false  // all values matched
    * }}}
    */
  private def isUseful(matrix: PatternMatrix, q: List[Pattern], root: Root): Boolean = {
    if (matrix.width == 0) {
      // Base case: zero columns. Useful iff no rows exist.
      return matrix.isEmpty
    }

    val sigma = headConstructors(matrix)
    val qHead = q.head

    if (isCompleteSignature(sigma, root)) {
      // Complete signature: check specialization for every constructor in sigma.
      // q is useful iff it is useful against at least one specialization.
      if (isWildcard(qHead)) {
        // Wildcard head: must check all constructors
        sigma.exists { ctor =>
          val specMatrix = specialize(ctor, matrix)
          val specQ = specializeVector(ctor, q).get
          isUseful(specMatrix, specQ, root)
        }
      } else {
        // Constructor head: only check the matching constructor
        val specMatrix = specialize(qHead, matrix)
        val specQ = specializeVector(qHead, q).get
        isUseful(specMatrix, specQ, root)
      }
    } else {
      // Incomplete signature.
      if (isWildcard(qHead)) {
        // Wildcard head: check default matrix
        val defMatrix = computeDefaultMatrix(matrix)
        val defQ = q.tail
        isUseful(defMatrix, defQ, root)
      } else {
        // Constructor head: check specialized matrix for this constructor
        // If the constructor is in sigma, specialize for it.
        // If not in sigma, it's definitely useful (it's a new constructor).
        if (sigma.exists(c => sameConstructor(c, qHead))) {
          val specMatrix = specialize(qHead, matrix)
          val specQ = specializeVector(qHead, q).get
          isUseful(specMatrix, specQ, root)
        } else {
          // Constructor not in sigma — check against default matrix
          // with the sub-patterns of q prepended
          val arity = patternArity(qHead)
          val defMatrix = computeDefaultMatrix(matrix)
          val subPats = qHead match {
            case Pattern.Tag(_, pats, _, _) => pats
            case Pattern.Tuple(pats, _, _) => pats.toList
            case Pattern.Record(pats, pat, _, _) => pats.map(_.pat) ::: List(pat)
            case _: Pattern.Cst => Nil
            case _ => Nil
          }
          val defQ = subPats ::: q.tail
          // The default matrix width is matrix.width - 1, but defQ has length arity + (q.length - 1)
          // We need to expand the default matrix rows to account for the arity.
          // Actually, for an incomplete signature with a constructor not in sigma,
          // the row can only match via wildcard rows. The default matrix drops the
          // first column for wildcard rows. But our q has expanded sub-patterns.
          // We need to specialize instead.
          val specMatrix = specialize(qHead, matrix)
          val specQ = specializeVector(qHead, q).get
          isUseful(specMatrix, specQ, root)
        }
      }
    }
  }

  // ─────────────────────────────────────────────────────────────
  //  Witness Construction
  // ─────────────────────────────────────────────────────────────

  /**
    * Computes a witness pattern (an example of an unmatched value) for a
    * non-exhaustive pattern matrix, or `None` if the matrix is exhaustive.
    *
    * Returns a list of `WitnessPattern` elements representing the columns.
    * For a single-column match, the list has one element.
    *
    * Example:
    * {{{
    *   matrix = [ [Some(true)] ]   // width = 1
    *   computeWitness(matrix, root) = Some(List(Tag(Some, [Literal(false)])))
    *
    *   matrix = [ [_] ]            // width = 1
    *   computeWitness(matrix, root) = None  // exhaustive
    * }}}
    */
  private def computeWitness(matrix: PatternMatrix, root: Root): Option[List[WitnessPattern]] = {
    if (matrix.width == 0) {
      // Base case: zero columns. Witness exists iff no rows.
      return if (matrix.isEmpty) Some(Nil) else None
    }

    val sigma = headConstructors(matrix)

    if (isCompleteSignature(sigma, root)) {
      // Complete: check each constructor; any witness suffices.
      sigma.collectFirst(Function.unlift { ctor =>
        val specMatrix = specialize(ctor, matrix)
        computeWitness(specMatrix, root).map { subWitness =>
          val arity = patternArity(ctor)
          val (args, rest) = subWitness.splitAt(arity)
          buildWitnessHead(ctor, args) :: rest
        }
      })
    } else {
      // Incomplete: try default matrix first.
      val defMatrix = computeDefaultMatrix(matrix)
      computeWitness(defMatrix, root).map { restWitness =>
        if (sigma.isEmpty) {
          // No constructors at all: prepend wildcard
          WitnessPattern.Wildcard :: restWitness
        } else {
          // Some constructors present but not complete: find a missing one
          val missing = computeMissingConstructor(sigma, root)
          missing :: restWitness
        }
      }
    }
  }

  /**
    * Constructs a `WitnessPattern` for the head constructor `ctor` with
    * the given `args` as sub-witness patterns.
    *
    * Example:
    * {{{
    *   buildWitnessHead(Tag(Some, [_]), [Wildcard]) = Tag(Some, [Wildcard])
    *   buildWitnessHead(Tuple([_, _]),  [Wildcard, Literal(1)]) = Tuple([Wildcard, Literal(1)])
    *   buildWitnessHead(Cst(true),      []) = Literal(true)
    * }}}
    */
  private def buildWitnessHead(ctor: Pattern, args: List[WitnessPattern]): WitnessPattern = ctor match {
    case Pattern.Tag(CaseSymUse(sym, _), _, _, _) => WitnessPattern.Tag(sym, args)
    case _: Pattern.Tuple                         => WitnessPattern.Tuple(args)
    case Pattern.Record(pats, _, _, _) =>
      val labels = pats.map(_.label)
      val (fieldArgs, tailArg) = args.splitAt(labels.length)
      WitnessPattern.Record(labels.zip(fieldArgs), tailArg.headOption.getOrElse(WitnessPattern.Wildcard))
    case Pattern.Cst(cst, _, _)                   => WitnessPattern.Literal(cst)
    case _                                        => WitnessPattern.Wildcard
  }

  /**
    * Synthesizes a witness for a constructor not present in `sigma`.
    *
    * For enums: finds a case not in sigma and fills args with wildcards.
    * For booleans: returns the missing `true` or `false`.
    * For infinite types: returns a wildcard.
    *
    * Example:
    * {{{
    *   // Given enum Color { Red, Grn, Blu } and sigma = [Red, Grn]:
    *   computeMissingConstructor(sigma, root) = Tag(Blu, [])
    *
    *   // Given sigma = [Cst(true)]:
    *   computeMissingConstructor(sigma, root) = Literal(false)
    * }}}
    */
  private def computeMissingConstructor(sigma: List[Pattern], root: Root): WitnessPattern = {
    sigma.head match {
      case Pattern.Tag(CaseSymUse(sym, _), _, _, _) =>
        val allCases = root.enums(sym.enumSym).cases
        val covered = sigma.collect { case Pattern.Tag(CaseSymUse(s, _), _, _, _) => s }.toSet
        allCases.collectFirst {
          case (caseSym, caze) if !covered.contains(caseSym) =>
            WitnessPattern.Tag(caseSym, List.fill(caze.tpes.length)(WitnessPattern.Wildcard))
        }.getOrElse(WitnessPattern.Wildcard)

      case Pattern.Cst(Constant.Bool(_), _, _) =>
        val hasTrue = sigma.exists { case Pattern.Cst(Constant.Bool(true), _, _) => true; case _ => false }
        if (hasTrue) WitnessPattern.Literal(Constant.Bool(false))
        else WitnessPattern.Literal(Constant.Bool(true))

      case _ => WitnessPattern.Wildcard
    }
  }

  // ─────────────────────────────────────────────────────────────
  //  Formatting
  // ─────────────────────────────────────────────────────────────

  /**
    * Formats a `WitnessPattern` as a human-readable string for error messages.
    *
    * Example:
    * {{{
    *   formatPattern(Wildcard)                     = "_"
    *   formatPattern(Literal(true))                = "true"
    *   formatPattern(Tag(Some, [Wildcard]))         = "Some(_)"
    *   formatPattern(Tag(None, []))                = "None"
    *   formatPattern(Tuple([Wildcard, Literal(1)])) = "(_, 1)"
    *   formatPattern(Record([(x, Wildcard)], Wildcard)) = "{ x = _ | _ }"
    * }}}
    */
  private def formatPattern(wp: WitnessPattern): String = wp match {
    case WitnessPattern.Wildcard      => "_"
    case WitnessPattern.Literal(cst)  => FormatConstant.formatConstant(cst)
    case WitnessPattern.Tag(sym, Nil) => sym.name
    case WitnessPattern.Tag(sym, args) =>
      sym.name + args.map(formatPattern).mkString("(", ", ", ")")
    case WitnessPattern.Tuple(elms) =>
      elms.map(formatPattern).mkString("(", ", ", ")")
    case WitnessPattern.Record(labels, tail) =>
      val labelStr = labels.map { case (l, p) => s"$l = ${formatPattern(p)}" }.mkString(", ")
      val tailStr = tail match {
        case WitnessPattern.Literal(Constant.RecordEmpty) => ""
        case t => s" | ${formatPattern(t)}"
      }
      "{ " + labelStr + tailStr + " }"
  }

  // ─────────────────────────────────────────────────────────────
  //  Check Entry Points
  // ─────────────────────────────────────────────────────────────

  /**
    * Checks a `match` expression for exhaustiveness and redundancy.
    *
    * **Exhaustiveness**: Builds a `PatternMatrix` from all unguarded rules and
    * calls `computeWitness`. If a witness is found, reports a
    * `NonExhaustiveMatchError`.
    *
    * **Redundancy**: Iterates rules in order, maintaining a matrix of preceding
    * unguarded rules. For each rule (guarded or not), checks `isUseful` against
    * the preceding matrix. If not useful, reports a `RedundantPatternError`.
    * Only unguarded rules are added to the preceding matrix (guarded rules
    * cannot make later rules redundant).
    */
  private def checkRules(exp: Expr, rules: List[TypedAst.MatchRule], root: Root)(implicit sctx: SharedContext): Unit = {
    // --- Exhaustiveness ---
    val unguardedPats = rules.filter(_.guard.isEmpty).map(r => List(r.pat))
    val exhaustMatrix = PatternMatrix(unguardedPats, 1)
    computeWitness(exhaustMatrix, root) match {
      case Some(witness) =>
        val patStr = if (witness.nonEmpty) formatPattern(witness.head) else "_"
        sctx.errors.add(PatMatchError.NonExhaustiveMatch(patStr, exp.loc))
      case None => ()
    }

    // --- Redundancy ---
    var precedingRows: List[List[Pattern]] = Nil
    for (rule <- rules) {
      val preceding = PatternMatrix(precedingRows, 1)
      val q = List(rule.pat)
      if (!isUseful(preceding, q, root)) {
        sctx.errors.add(PatMatchError.RedundantPattern(rule.pat.loc))
      }
      // Only add unguarded rules to the preceding matrix
      if (rule.guard.isEmpty) {
        precedingRows = precedingRows :+ q
      }
    }
  }

  /**
    * Checks `ParYield` fragments for exhaustiveness.
    *
    * Each fragment's pattern is checked individually (single-column matrix).
    * Redundancy is not checked for `ParYield` (each fragment is independent).
    */
  private def checkFrags(frags: List[ParYieldFragment], root: Root, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    frags.foreach { f =>
      val matrix = PatternMatrix(List(List(f.pat)), 1)
      computeWitness(matrix, root) match {
        case Some(witness) =>
          val patStr = if (witness.nonEmpty) formatPattern(witness.head) else "_"
          sctx.errors.add(PatMatchError.NonExhaustiveMatch(patStr, loc))
        case None => ()
      }
    }
  }

  // ─────────────────────────────────────────────────────────────
  //  Visitor Structure
  // ─────────────────────────────────────────────────────────────

  /**
    * Entry point. Runs the pattern match exhaustiveness and redundancy phase.
    *
    * Uses `ChangeSet` for incremental compilation and `ParOps` for parallelism.
    * Returns the (unmodified) root together with any errors found.
    */
  def run(root: Root, oldRoot: Root, changeSet: ChangeSet)(implicit flix: Flix): (Root, List[CompilationMessage]) =
    flix.phaseNew("PatMatch") {
      implicit val r: Root = root
      implicit val sctx: SharedContext = SharedContext.mk()

      val defs = changeSet.updateStaleValues(root.defs, oldRoot.defs)(ParOps.parMapValues(_)(visitDef))
      val traits = changeSet.updateStaleValues(root.traits, oldRoot.traits)(ParOps.parMapValues(_)(visitTrait))
      val instances = changeSet.updateStaleValueLists(root.instances, oldRoot.instances,
        (i1: TypedAst.Instance, i2: TypedAst.Instance) => i1.tpe.typeConstructor == i2.tpe.typeConstructor
      )(ParOps.parMapValueList(_)(visitInstance))

      (root.copy(defs = defs, traits = traits, instances = instances), sctx.errors.asScala.toList)
    }

  /** Visits a definition, checking all match expressions in its body. */
  private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext, root: Root, flix: Flix): TypedAst.Def = {
    visitExp(defn.exp)
    defn
  }

  /** Visits a trait, checking match expressions in all signature default implementations. */
  private def visitTrait(trt: TypedAst.Trait)(implicit sctx: SharedContext, root: Root, flix: Flix): TypedAst.Trait = {
    trt.sigs.flatMap(_.exp).foreach(visitExp)
    trt
  }

  /** Visits an instance, checking match expressions in all of its definitions. */
  private def visitInstance(inst: TypedAst.Instance)(implicit sctx: SharedContext, root: Root, flix: Flix): TypedAst.Instance = {
    inst.defs.foreach(visitDef)
    inst
  }

  /**
    * Recursively visits all sub-expressions of `tast`, checking `Match` and
    * `ParYield` expressions for exhaustiveness and redundancy.
    *
    * `RestrictableChoose` and `ExtMatch` are skipped per requirement (f).
    */
  private def visitExp(tast: Expr)(implicit sctx: SharedContext, root: Root, flix: Flix): Unit = {
    tast match {
      case Expr.Var(_, _, _) => ()

      case Expr.Hole(_, _, _, _, _) => ()

      case Expr.HoleWithExp(exp, _, _, _, _) => visitExp(exp)

      case Expr.OpenAs(_, exp, _, _) => visitExp(exp)

      case Expr.Use(_, _, exp, _) => visitExp(exp)

      case Expr.Cst(_, _, _) => ()

      case Expr.Lambda(_, body, _, _) => visitExp(body)

      case Expr.ApplyClo(exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.ApplyDef(_, exps, _, _, _, _, _) => exps.foreach(visitExp)

      case Expr.ApplyLocalDef(_, exps, _, _, _, _) => exps.foreach(visitExp)

      case Expr.ApplyOp(_, exps, _, _, _) => exps.foreach(visitExp)

      case Expr.ApplySig(_, exps, _, _, _, _, _, _) => exps.foreach(visitExp)

      case Expr.Unary(_, exp, _, _, _) => visitExp(exp)

      case Expr.Binary(_, exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.Let(_, exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.Region(_, _, exp, _, _, _) => visitExp(exp)

      case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)
        visitExp(exp3)

      case Expr.Stm(exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.Discard(exp, _, _) => visitExp(exp)

      case Expr.Match(exp, rules, _, _, _) =>
        visitExp(exp)
        rules.foreach { r =>
          visitExp(r.exp)
          r.guard.foreach(visitExp)
        }
        checkRules(exp, rules, root)

      case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
        visitExp(exp)
        rules.foreach(r => visitExp(r.exp))

      case Expr.ExtMatch(exp, rules, _, _, _) =>
        visitExp(exp)
        rules.foreach(r => visitExp(r.exp))

      case Expr.Tag(_, exps, _, _, _) => exps.foreach(visitExp)

      case Expr.RestrictableTag(_, exps, _, _, _) => exps.foreach(visitExp)

      case Expr.ExtTag(_, exps, _, _, _) => exps.foreach(visitExp)

      case Expr.Tuple(elms, _, _, _) => elms.foreach(visitExp)

      case Expr.RecordSelect(base, _, _, _, _) => visitExp(base)

      case Expr.RecordExtend(_, value, rest, _, _, _) =>
        visitExp(value)
        visitExp(rest)

      case Expr.RecordRestrict(_, rest, _, _, _) => visitExp(rest)

      case Expr.ArrayLit(exps, exp, _, _, _) =>
        visitExp(exp)
        exps.foreach(visitExp)

      case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)
        visitExp(exp3)

      case Expr.ArrayLoad(base, index, _, _, _) =>
        visitExp(base)
        visitExp(index)

      case Expr.ArrayStore(base, index, elm, _, _) =>
        visitExp(base)
        visitExp(index)
        visitExp(elm)

      case Expr.ArrayLength(base, _, _) => visitExp(base)

      case Expr.StructNew(_, fields, region, _, _, _) =>
        region.foreach(visitExp)
        fields.foreach { case (_, v) => visitExp(v) }

      case Expr.StructGet(exp, _, _, _, _) => visitExp(exp)

      case Expr.StructPut(exp1, _, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.VectorLit(exps, _, _, _) => exps.foreach(visitExp)

      case Expr.VectorLoad(exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.VectorLength(exp, _) => visitExp(exp)

      case Expr.Ascribe(exp, _, _, _, _, _) => visitExp(exp)

      case Expr.InstanceOf(exp, _, _) => visitExp(exp)

      case Expr.CheckedCast(_, exp, _, _, _) => visitExp(exp)

      case Expr.UncheckedCast(exp, _, _, _, _, _) => visitExp(exp)

      case Expr.Unsafe(exp, _, _, _, _, _) => visitExp(exp)

      case Expr.Without(exp, _, _, _, _) => visitExp(exp)

      case Expr.TryCatch(exp, rules, _, _, _) =>
        visitExp(exp)
        rules.foreach(r => visitExp(r.exp))

      case TypedAst.Expr.Throw(exp, _, _, _) => visitExp(exp)

      case Expr.Handler(_, rules, _, _, _, _, _) =>
        rules.foreach(r => visitExp(r.exp))

      case Expr.RunWith(exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.InvokeConstructor(_, args, _, _, _) => args.foreach(visitExp)

      case Expr.InvokeSuperConstructor(_, args, _, _, _) => args.foreach(visitExp)

      case Expr.InvokeMethod(_, exp, args, _, _, _) =>
        visitExp(exp)
        args.foreach(visitExp)

      case Expr.InvokeSuperMethod(_, args, _, _, _) => args.foreach(visitExp)

      case Expr.InvokeStaticMethod(_, args, _, _, _) => args.foreach(visitExp)

      case Expr.GetField(_, exp, _, _, _) => visitExp(exp)

      case Expr.PutField(_, exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.GetStaticField(_, _, _, _) => ()

      case Expr.PutStaticField(_, exp, _, _, _) => visitExp(exp)

      case Expr.NewObject(_, _, _, _, constructors, methods, _) =>
        constructors.foreach(c => visitExp(c.exp))
        methods.foreach(m => visitExp(m.exp))

      case Expr.NewChannel(exp, _, _, _) => visitExp(exp)

      case Expr.GetChannel(exp, _, _, _) => visitExp(exp)

      case Expr.PutChannel(exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.SelectChannel(rules, default, _, _, _) =>
        rules.foreach { r =>
          visitExp(r.exp)
          visitExp(r.chan)
        }
        default.foreach(visitExp)

      case Expr.Spawn(exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.ParYield(frags, exp, _, _, loc) =>
        visitExp(exp)
        frags.foreach(f => visitExp(f.exp))
        checkFrags(frags, root, loc)

      case Expr.Lazy(exp, _, _) => visitExp(exp)

      case Expr.Force(exp, _, _, _) => visitExp(exp)

      case Expr.FixpointConstraintSet(cs, _, _) => cs.foreach(visitConstraint)

      case Expr.FixpointLambda(_, exp, _, _, _) => visitExp(exp)

      case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
        visitExp(exp1)
        visitExp(exp2)

      case Expr.FixpointQueryWithProvenance(exps, select, _, _, _, _) =>
        exps.foreach(visitExp)
        visitHeadPred(select)

      case Expr.FixpointSolveWithProject(exps, _, _, _, _, _) => exps.foreach(visitExp)

      case Expr.FixpointQueryWithSelect(exps, queryExp, selects, from, where, _, _, _, _) =>
        exps.foreach(visitExp)
        visitExp(queryExp)
        selects.foreach(visitExp)
        from.foreach(visitBodyPred)
        where.foreach(visitExp)

      case Expr.FixpointInjectInto(exps, _, _, _, _) => exps.foreach(visitExp)

      case Expr.Error(_, _, _) => ()
    }
  }

  /** Visits a Datalog constraint, recursing into head and body predicates. */
  private def visitConstraint(c0: TypedAst.Constraint)(implicit sctx: SharedContext, root: Root, flix: Flix): Unit = c0 match {
    case TypedAst.Constraint(_, head0, body0, _) =>
      visitHeadPred(head0)
      body0.foreach(visitBodyPred)
  }

  /** Visits a head predicate, recursing into its term expressions. */
  private def visitHeadPred(h0: TypedAst.Predicate.Head)(implicit sctx: SharedContext, root: Root, flix: Flix): Unit = h0 match {
    case TypedAst.Predicate.Head.Atom(_, _, terms, _, _) => terms.foreach(visitExp)
  }

  /** Visits a body predicate, recursing into guard/functional expressions. */
  private def visitBodyPred(b0: TypedAst.Predicate.Body)(implicit sctx: SharedContext, root: Root, flix: Flix): Unit = b0 match {
    case TypedAst.Predicate.Body.Atom(_, _, _, _, _, _, _) => ()
    case TypedAst.Predicate.Body.Guard(exp, _) => visitExp(exp)
    case TypedAst.Predicate.Body.Functional(_, exp, _) => visitExp(exp)
  }

  // ─────────────────────────────────────────────────────────────
  //  Shared Context
  // ─────────────────────────────────────────────────────────────

  /** Companion object for [[SharedContext]]. */
  private object SharedContext {
    /** Returns a fresh shared context. */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A thread-safe container for errors collected during traversal.
    *
    * @param errors The compilation messages found so far.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[CompilationMessage])
}
