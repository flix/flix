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

mod Fixpoint.Phase.Simplifier {
    use Fixpoint.Ast.Ram.{BoolExp, Facts, Predicates, RamProgram, RamStmt, RelOp, RelSym, RowVar}
    use Fixpoint.Ast.Ram.RamTerm.RowLoad
    use Fixpoint.Predicate.{getRelSymAsType, PredType}

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
    @Internal
    pub def simplifyProgram(program: RamProgram): RamProgram = region rc {match program {
        case RamProgram.Program(stmt, facts, predicates, index) =>
            let nonEmpty = facts |>
                Map.filter(Vector.nonEmpty) |>
                Map.foldRightWithKey(k -> _ -> Set.insert(k), Set#{}) |>
                Set.map(rel -> getRelSymAsType(rel, PredType.Full, predicates));
            let _ = Array.empty(rc, 1);
            RamProgram.Program(simplifyStmt(stmt, nonEmpty, predicates), facts, predicates, index)
    }}

    ///
    /// Simplifies `stmt` by removing redundant code.
    ///
    /// `nonEmpty` is the set of `RelSym` whose relation is non-empty.
    ///
    /// `Predicates` is a structure for interacting with predicate symbols.
    ///
    @Internal
    pub def simplifyStmt(stmt: RamStmt, nonEmpty: Set[RelSym], predicates: Predicates): RamStmt =
        Option.getWithDefault(RamStmt.Seq(Vector#{}), simplifyHelper(nonEmpty, predicates, false, stmt))

    ///
    /// Helper function for `simplifyStmt`.
    ///
    def simplifyHelper(nonEmpty: Set[RelSym], predicates: Predicates, inUntil: Bool, stmt: RamStmt): Option[RamStmt] = match stmt {
        case RamStmt.Insert(op) =>
            simplifyOp(nonEmpty, predicates, inUntil, Set#{}, op) |>
            Option.map(RamStmt.Insert)
        case RamStmt.MergeInto(_, _) => Some(stmt)
        case RamStmt.Swap(_, _) => Some(stmt)
        case RamStmt.Purge(_) => Some(stmt)
        case RamStmt.Seq(xs) =>
            Some(RamStmt.Seq(Vector.filterMap(simplifyHelper(nonEmpty, predicates, inUntil), xs)))
        case RamStmt.Par(xs) =>
            Some(RamStmt.Par(Vector.filterMap(simplifyHelper(nonEmpty, predicates, inUntil), xs)))
        case RamStmt.Until(test, body) =>
            match simplifyHelper(nonEmpty, predicates, true, body) {
                case None => None
                case Some(simplifiedBody) =>
                    if(isOnlyMergeSwapPurge(simplifiedBody)) {
                        None
                    } else {
                        Some(RamStmt.Until(test, simplifiedBody))
                    }
            }
        case RamStmt.EstimateJoinSize(_, _, _, _) => Some(stmt)
        case RamStmt.Comment(_) => Some(stmt)
    }

    ///
    /// Simplify `op` by removing redundant code. See `simplifyStmt`.
    ///
    /// `inUntil` is true if `op` appears in an `Until` statement.
    ///
    /// `seenPreds` is the set of `RelSym` seen so far.
    ///
    def simplifyOp(nonEmpty: Set[RelSym], predicates: Predicates, inUntil: Bool, seenPreds: Set[RelSym], op: RelOp): Option[RelOp] =
        let recurseNewRel = rel -> simplifyOp(nonEmpty, predicates, inUntil, Set.insert(rel, seenPreds));
        let recurse = simplifyOp(nonEmpty, predicates, inUntil, seenPreds);
        match op {
            case RelOp.Search(rowVar, ramSym, body) =>
                Option.map(newBody -> RelOp.Search(rowVar, ramSym, newBody), recurseNewRel(ramSym, body))
            case RelOp.Query(rowVar, ramSym, qry, _, body) =>
                let f = nb -> match ramSym {
                    case _ => Some(RelOp.Query(rowVar, ramSym, qry, -1, nb))
                };
                forM (
                    newBody <- recurseNewRel(ramSym, body);
                    res <- f(newBody)
                ) yield res
            case RelOp.Functional(rowVar, f, terms, body, arity) =>
                forM(
                    newBody <- recurse(body)
                ) yield RelOp.Functional(rowVar, f, terms, newBody, arity)
            case RelOp.Project(_, rel) =>
                // If we are not in an until and we insert in the new relation for `rel`,
                // we depend on `rel`, and `rel` is empty beforehand the code is dead.
                let fullRel = getRelSymAsType(rel, PredType.Full, predicates);
                let dependOnRel = Set.memberOf(fullRel, seenPreds);
                let relIsEmpty = (not Set.memberOf(fullRel, nonEmpty));
                let isDead = (not inUntil) and dependOnRel and relIsEmpty;
                if(isDead) None
                else Some(op)
            case RelOp.If(test, then) =>
                let (memberOf, rest) =
                    // Delete checks of the form `x[i] == x[i]`.
                    Vector.filter(e -> match e {
                        case BoolExp.Eq(RowLoad(lhs1, lhs2, _), RowLoad(rhs1, rhs2, _)) => (lhs1, lhs2) != (rhs1, rhs2)
                        case _ => true
                    }, test) |>
                    // Partition into membership tests and rest.
                    Vector.partition(e -> match e {
                        case BoolExp.NotMemberOf(_, _) => true
                        case _ => false
                    });
                let newTest = Vector.append(rest, memberOf);
                // Simplify `if () then body` to `body`.
                match recurse(then) {
                    case None => None
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
        case RamStmt.Insert(_) => false
        case RamStmt.MergeInto(_, _) => true
        case RamStmt.Swap(_, _) => true
        case RamStmt.Purge(_) => true
        case RamStmt.Seq(xs) => Vector.forAll(isOnlyMergeSwapPurge, xs)
        case RamStmt.Par(xs) => Vector.forAll(isOnlyMergeSwapPurge, xs)
        case RamStmt.Until(_, _) => false
        case RamStmt.EstimateJoinSize(_, _, _, _) => false
        case RamStmt.Comment(_) => true
    }

}
