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
    use Fixpoint.Ast.Ram.{RamStmt, RelOp, RelSym, RamTerm, BoolExp, RowVar, Facts, RamProgram}
    use Fixpoint.Ast.Ram.RamTerm.{RowLoad, Lit, Meet}

    ///
    /// Optimize and simplify `stmt` by deleting redundant code and reordering code.
    /// Examples of redundancy include `x[i] == x[i]` or `x ⊓ y ≤ x`.
    /// Reordering means `(0, 1) ∉ Path ∧ x[1] = y[0]` would be swapped `x[1] = y[0] ∧ (0, 1) ∉ Path`.
    /// A simple static analysis also reveals that the following join-loop is redundant
    /// in stratum `j` if `C` is computed in stratum `i` and `i < j`:
    ///     search B$1 ∈ B do
    ///         search C$2 ∈ ΔC do
    ///             if (B$1[0] == C$2[0] ∧ (B$1[0]) ∉ A ∧ (B$1[0]) ∉ R) then
    ///                 project (B$1[0]) into ΔR'
    ///             end
    ///         end
    ///     end
    ///
    @Internal
    pub def simplifyProgram(program: RamProgram): RamProgram = match program {
        case RamProgram.Program(stmt, facts, meta, index) => RamProgram.Program(simplifyStmt(stmt), facts, meta, index)
    }
    @Internal
    pub def simplifyStmt(stmt: RamStmt): RamStmt =
        Option.getWithDefault(RamStmt.Seq(Vector#{}), simplifyHelper(stmt))

    def simplifyHelper(stmt: RamStmt): Option[RamStmt] = match stmt {
        case RamStmt.Insert(op) =>
            forM (
                newOp <- simplifyOp(op)
            ) yield {
                match newOp {
                    //
                    // DANGER! WILL ROBINSON! The following code is incorrect when a relation is being merged into a lattice.
                    //
                    // See #4719.
                    //
                    // // Rewrite join loops that copy one relation into another into a `merge` statement.
                    // // search b ∈ B do
                    // //   project (b[0], b[1]) into A
                    // // end
                    // // ==>
                    // // merge B into A
                    // // If A and B have the same arity.
                    // case RelOp.Search(varB, relB, RelOp.Project(tuple, relA)) =>
                    //     use Fixpoint.Ram.arityOf;
                    //     let isCopy =
                    //         Vector.map(t -> match t {
                    //             case RamTerm.RowLoad(var, i) => if (varB == var) i else -1
                    //             case _ => -1
                    //         }, tuple) == Vector.range(0, arityOf(relB));
                    //     if (isCopy)
                    //         RamStmt.Merge(relB, relA)
                    //     else
                    //         RamStmt.Insert(newOp)
                    case _ => RamStmt.Insert(newOp)
                }
            }
        case RamStmt.Merge(_, _) => Some(stmt)
        case RamStmt.Swap(_, _) => Some(stmt)
        case RamStmt.Purge(_) => Some(stmt)
        case RamStmt.Seq(xs) =>
            Some(RamStmt.Seq(Vector.filterMap(simplifyHelper, xs)))
        case RamStmt.Until(test, body) =>
            match simplifyHelper(body) {
                case None => None
                case Some(simplifiedBody) =>
                    if(isOnlyMergeSwapPurge(simplifiedBody)) {
                        None
                    } else {
                        Some(RamStmt.Until(test, simplifiedBody))
                    }
            }
        case RamStmt.Comment(_) => Some(stmt)
        case RamStmt.Par(xs) => Some(RamStmt.Par(Vector.filterMap(simplifyHelper, xs)))
        case RamStmt.EstimateJoinSize(_, _, _, _) => Some(stmt)
    }

    def simplifyOp(op: RelOp): Option[RelOp] = match op {
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
            if (Vector.isEmpty(newTest))
                Some(then)
            else
                Some(RelOp.If(newTest, then))
        case RelOp.Search(rowVar, ramSym, body) =>
            Option.map(newBody -> RelOp.Search(rowVar, ramSym, newBody), simplifyOp(body))
        case RelOp.Query(rowVar, ramSym, qry, _, body) =>
            let f = nb -> match ramSym {
                case _ => Some(RelOp.Query(rowVar, ramSym, qry, -1, nb))
            };
            forM (
                newBody <- simplifyOp(body);
                res <- f(newBody)
            ) yield res
        // case RelOp.Query(rowVar, ramSym, qry, body) =>
        //     let f = nb -> match ramSym {
        //         // case RelSym.Delta(_, _, _) =>
        //         //     if (Set.memberOf(ramSym, stratum))
        //         //         Some(RelOp.Query(rowVar, ramSym, qry, nb))
        //         //     else
        //         //         None
        //         case _ => Some(RelOp.Query(rowVar, ramSym, qry, nb))
        //     };
        //     forM (
        //         newBody <- simplifyOp(stratum, body);
        //         res <- f(newBody)
        //     ) yield res
        case RelOp.Functional(rowVar, f, terms, body, arity) =>
            forM(
                newBody <- simplifyOp(body)
            ) yield RelOp.Functional(rowVar, f, terms, newBody, arity)
        case RelOp.Project(_, _)    => Some(op)
    }

    def isOnlyMergeSwapPurge(r: RamStmt): Bool = match r {
        case RamStmt.Insert(_) => false
        case RamStmt.Merge(_, _) => true
        case RamStmt.Swap(_, _) => true
        case RamStmt.Purge(_) => true
        case RamStmt.Seq(xs) => Vector.forAll(isOnlyMergeSwapPurge, xs)
        case RamStmt.Until(_, _) => false
        case RamStmt.Comment(_) => true
        case RamStmt.Par(xs) => Vector.forAll(isOnlyMergeSwapPurge, xs)
        case RamStmt.EstimateJoinSize(_, _, _, _) => false
    }
    
}
