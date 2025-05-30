/*
 * Copyright 2021 Benjamin Dahse
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

    use Fixpoint.Ast.Ram.{RamStmt, RelOp, RamSym, RamTerm, BoolExp, RowVar}
    use Fixpoint.Ast.Ram.RamTerm.{RowLoad, LoadLatVar, Lit, Meet}

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
    pub def simplifyStmt(stmt: RamStmt): RamStmt =
        Option.getWithDefault(RamStmt.Seq(Vector#{}), simplifyHelper(Set#{}, stmt))

    def simplifyHelper(stratum: Set[RamSym], stmt: RamStmt): Option[RamStmt] = match stmt {
        case RamStmt.Insert(op) =>
            forM (
                newOp <- simplifyOp(stratum, op)
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
        case RamStmt.Assign(_, _) => Some(stmt)
        case RamStmt.Purge(_) => Some(stmt)
        case RamStmt.Seq(xs) =>
            Some(RamStmt.Seq(Vector.filterMap(simplifyHelper(stratum), xs)))
        case RamStmt.Until(test, body) =>
            let newStratum = Vector.foldLeft(acc -> e -> match e {
                case BoolExp.Empty(ramSym) => Set.insert(ramSym, acc)
                case _ => acc
            }, Set#{}, test);
            simplifyHelper(newStratum, body) |>
            Option.map(newBody -> RamStmt.Until(test, newBody))
        case RamStmt.Comment(_) => Some(stmt)
    }

    def simplifyOp(stratum: Set[RamSym], op: RelOp): Option[RelOp] = match op {
        case RelOp.If(test, then) =>
            let (memberOf, rest) =
                // Delete checks of the form `x[i] == x[i]` and `x ≤ x` and `x ⊓ y ≤ x`.
                Vector.filter(e -> match e {
                    case BoolExp.Eq(RowLoad(lhs1, lhs2), RowLoad(rhs1, rhs2)) => (lhs1, lhs2) != (rhs1, rhs2)
                    case BoolExp.Leq(_, lhs, LoadLatVar(rhsVar)) =>
                        let lhsVars = ramTermVars(lhs);
                        not Set.memberOf(rhsVar, lhsVars)
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
            let f = nb -> match ramSym {
                case RamSym.Delta(_, _, _) =>
                    if (Set.memberOf(ramSym, stratum))
                        Some(RelOp.Search(rowVar, ramSym, nb))
                    else
                        None
                case _ => Some(RelOp.Search(rowVar, ramSym, nb))
            };
            forM (
                newBody <- simplifyOp(stratum, body);
                res <- f(newBody)
            ) yield res
        case RelOp.Query(rowVar, ramSym, qry, body) =>
            let f = nb -> match ramSym {
                case RamSym.Delta(_, _, _) =>
                    if (Set.memberOf(ramSym, stratum))
                        Some(RelOp.Query(rowVar, ramSym, qry, nb))
                    else
                        None
                case _ => Some(RelOp.Query(rowVar, ramSym, qry, nb))
            };
            forM (
                newBody <- simplifyOp(stratum, body);
                res <- f(newBody)
            ) yield res
        case RelOp.Functional(rowVar, f, terms, body) =>
            forM(
                newBody <- simplifyOp(stratum, body)
            ) yield RelOp.Functional(rowVar, f, terms, newBody)
        case RelOp.Project(_, _)    => Some(op)
    }

    ///
    /// Returns the set of variables that occur in `term`.
    ///
    def ramTermVars(term: RamTerm): Set[RowVar] = match term {
        case RamTerm.Lit(_) => Set#{}
        case RamTerm.RowLoad(var, _) => Set#{var}
        case RamTerm.LoadLatVar(var) => Set#{var}
        case RamTerm.Meet(_, lhs, rhs) => Set.union(ramTermVars(lhs), ramTermVars(rhs))
        case RamTerm.App0(_) => Set#{}
        case RamTerm.App1(_, t) => ramTermVars(t)
        case RamTerm.App2(_, t1, t2) => Set.union(ramTermVars(t1), ramTermVars(t2))
        case RamTerm.App3(_, t1, t2, t3) =>
            let v1 = ramTermVars(t1);
            let v2 = ramTermVars(t2);
            let v3 = ramTermVars(t3);
            Set.union(v1, Set.union(v2, v3))
        case RamTerm.App4(_, t1, t2, t3, t4) =>
            let v1 = ramTermVars(t1);
            let v2 = ramTermVars(t2);
            let v3 = ramTermVars(t3);
            let v4 = ramTermVars(t4);
            Set.union(v1, Set.union(v2, Set.union(v3, v4)))
        case RamTerm.App5(_, t1, t2, t3, t4, t5) =>
            let v1 = ramTermVars(t1);
            let v2 = ramTermVars(t2);
            let v3 = ramTermVars(t3);
            let v4 = ramTermVars(t4);
            let v5 = ramTermVars(t5);
            Set.union(v1, Set.union(v2, Set.union(v3, Set.union(v4, v5))))
    }
}
