/*
 * Copyright 2021 Benjamin Dahse
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

pub mod Fixpoint3.Phase.Simplifier {
    use Fixpoint3.Ast.Ram.{BoolExp, Facts, Predicates, RamProgram, RamStmt, RelOp, RelSym, RowVar}
    use Fixpoint3.Ast.Ram.RamTerm.RowLoad
    use Fixpoint3.Predicate.{getRelSymAsType, PredType}

    ///
    /// Optimize and simplify `stmt` by deleting redundant code and reordering code.
    /// Examples of redundancy include `x[i] == x[i]`.
    /// Reordering means `(0, 1) ∉ Path ∧ x[1] = y[0]` would be swapped `x[1] = y[0] ∧ (0, 1) ∉ Path`.
    /// A simple static analysis also reveals that the following join-loop is redundant
    /// in stratum `j` if `C` is computed in stratum `i` and `i < j`:
    /// ```
    /// search x ∈ B do
    ///     search y ∈ ΔC do
    ///         if (x[0] == y[0] ∧ (x[0]) ∉ A ∧ (x[0]) ∉ R) then
    ///             project (x[0]) into ΔR'
    ///         end
    ///     end
    /// end
    /// ```
    ///
    /// The returned RAM program is always the same size or smaller.
    ///
    pub def simplifyProgram(program: RamProgram): RamProgram = unsafe IO {
        match program {
            case RamProgram.Program(stmt, facts, predicates, index, typeInfo) =>
                // Compute the a set of all relations which are initialized with facts, i.e. have some EDB facts.
                let filterOutEmpty = fact -> fact
                    |> Map.filter(x -> (not BPlusTree.isEmpty(x)))
                    |> Map.foldRightWithKey(k -> _ -> Set.insert(k), Set#{})
                    |> Set.map(rel -> getRelSymAsType(rel, PredType.Full, predicates));
                let nonEmpty = filterOutEmpty(fst(facts)) `Set.union` filterOutEmpty(snd(facts));
                RamProgram.Program(simplifyStmt(stmt, nonEmpty, predicates), facts, predicates, index, typeInfo)
        }
    }

    ///
    /// Simplifies `stmt` by removing redundant code.
    ///
    /// `nonEmpty` is the set of `RelSym` whose relation is non-empty.
    ///
    /// `Predicates` is a structure for interacting with predicate symbols.
    ///
    pub def simplifyStmt(stmt: RamStmt, nonEmpty: Set[RelSym], predicates: Predicates): RamStmt =
        Option.getWithDefault(RamStmt.Seq(Vector#{}), simplifyHelper(nonEmpty, predicates, false, stmt))

    ///
    /// Helper function for `simplifyStmt`.
    ///
    /// `nonEmpty` is the set of `RelSym` whose relation is non-empty.
    ///
    /// `Predicates` is a structure for interacting with predicate symbols.
    ///
    /// `inUntil` is true iff `stmt` is in the body of an `Until` stmt.
    ///
    /// Returns `None` when `stmt` is discovered to be redundant.
    ///
    /// Returns `Some(v)` where `v` semantically equivalent to `stmt` if we cannot
    /// conclude that `stmt` is redundant.
    ///
    /// The redundant examples handled directly by `simplifyHelper` is restricted to `Until(test, body)` where
    /// `body` can be simplified to only contain `Merge`, `Swap` and `Purge`.
    ///
    /// A simplification of an `Insert` can also lead to `None`, though this is handled by `simplifyOp`.
    ///
    def simplifyHelper(nonEmpty: Set[RelSym], predicates: Predicates, inUntil: Bool, stmt: RamStmt): Option[RamStmt] = match stmt {
        case RamStmt.Insert(op) =>
            simplifyOp(nonEmpty, predicates, inUntil, Set#{}, op) |>
                Option.map(RamStmt.Insert)
        case RamStmt.MergeInto(_, _) => Some(stmt)
        case RamStmt.Swap(_, _)      => Some(stmt)
        case RamStmt.Purge(_)        => Some(stmt)
        case RamStmt.Seq(xs) =>
            Vector.filterMap(simplifyHelper(nonEmpty, predicates, inUntil), xs)
                |> RamStmt.Seq
                |> Some
        case RamStmt.Par(xs) =>
            Vector.filterMap(simplifyHelper(nonEmpty, predicates, inUntil), xs)
                |> RamStmt.Par
                |> Some
        case RamStmt.Until(test, body) =>
            match simplifyHelper(nonEmpty, predicates, true, body) {
                case None                 => None
                case Some(simplifiedBody) =>
                    if (isOnlyMergeSwapPurge(simplifiedBody)) {
                        None
                    } else {
                        Some(RamStmt.Until(test, simplifiedBody))
                    }
            }
        case RamStmt.Comment(_) => Some(stmt)
    }

    ///
    /// Simplify `op` by removing redundant code. See `simplifyStmt`.
    ///
    /// `inUntil` is true if `op` appears in an `Until` statement.
    ///
    /// `seenPreds` is the set of `RelSym` seen so far.
    ///
    /// Returns `None` when children are simplified to `None` or for certain `Project`
    /// statements not in `Until` bodies depending on themselves. Consider the example
    /// ```
    /// search x ∈ B do
    ///     search y ∈ A do
    ///         project (x[0], y[0]) into ΔA'
    ///     end
    /// end;
    /// Merge(ΔA', A);
    /// Swap(ΔA', ΔA);
    /// Until(...) {...}
    /// ```
    ///
    /// Assuming `A` is empty/has no EDB facts then the naive loop is redundant.
    /// This optimization is sound as `A` is only written to in merge statements and they always occur
    /// after the naive `Project` statements generated by the rules with `A` as head. We cannot remove any part of
    /// the semi-naive `Until`-loop, as other rules could populate `A`/`ΔA`.
    ///
    /// Returns `Some(v)` otherwise, where `v` is a simplifcation of `op`.
    ///
    def simplifyOp(nonEmpty: Set[RelSym], predicates: Predicates, inUntil: Bool, seenPreds: Set[RelSym], op: RelOp): Option[RelOp] =
        let recurseNewRel = rel -> simplifyOp(nonEmpty, predicates, inUntil, Set.insert(rel, seenPreds));
        let recurse = simplifyOp(nonEmpty, predicates, inUntil, seenPreds);
        match op {
            case RelOp.Search(rowVar, relSym, t, body) =>
                recurseNewRel(relSym, body) |> Option.map(newBody -> RelOp.Search(rowVar, relSym, t, newBody))
            case RelOp.Query(rowVar, relSym, qry, _, t, body) =>
                recurseNewRel(relSym, body) |> Option.map(RelOp.Query(rowVar, relSym, qry, -1, t))
            case RelOp.Functional(rowVar, f, terms, body, arity, t) =>
                recurse(body) |> Option.map(newBody -> RelOp.Functional(rowVar, f, terms, newBody, arity, t))
            case RelOp.Project(_, rel, _) =>
                // If we are not in an until and we insert in the new relation for `rel`,
                // we depend on `rel`, and `rel` is empty beforehand the code is dead.
                // See the example above in the doc comment for what this removes.
                let fullRel = getRelSymAsType(rel, PredType.Full, predicates);
                let dependOnRel = Set.memberOf(fullRel, seenPreds);
                let relIsEmpty = (not Set.memberOf(fullRel, nonEmpty));
                let isDead = (not inUntil) and dependOnRel and relIsEmpty;
                if (isDead) None else Some(op)
            case RelOp.If(test, then) =>
                let (memberOf, rest) = test
                    // Delete checks of the form `x[i] == x[i]`.
                    |> Vector.filter(e -> match e {
                        case BoolExp.Eq(RowLoad(lhs1, lhs2, _, _), RowLoad(rhs1, rhs2, _, _)) =>
                            (lhs1, lhs2) != (rhs1, rhs2)
                        case _ => true
                    })
                    // Partition into membership tests and rest.
                    |> Vector.partition(e -> match e {
                        case BoolExp.NotMemberOf(_, _, _) => true
                        case _                            => false
                    });
                let newTest = rest ++ memberOf;
                // Simplify `if () then body` to `body`.
                match recurse(then) {
                    case None          => None
                    case Some(newThen) =>
                        if (Vector.isEmpty(newTest))
                            Some(newThen)
                        else
                            Some(RelOp.If(newTest, newThen))
                }
    }

    ///
    /// Returns true if `r` is a `Merge`, `Swap`, or `Purge` statement or contains any one
    /// of them.
    ///
    def isOnlyMergeSwapPurge(r: RamStmt): Bool = match r {
        case RamStmt.Insert(_)       => false
        case RamStmt.MergeInto(_, _) => true
        case RamStmt.Swap(_, _)      => true
        case RamStmt.Purge(_)        => true
        case RamStmt.Seq(xs)         => Vector.forAll(isOnlyMergeSwapPurge, xs)
        case RamStmt.Par(xs)         => Vector.forAll(isOnlyMergeSwapPurge, xs)
        case RamStmt.Until(_, _)     => false
        case RamStmt.Comment(_)      => true
    }

}
