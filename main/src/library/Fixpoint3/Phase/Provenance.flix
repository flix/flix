/*
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

///
/// This module augments the program to track when and how facts were created.
///
/// EDB facts will be augmented with time/depth `0` and rule `-1`.
///
/// The program is augmented to compute ensure that computed facts track which
/// rule created them and at which depth they were time/created.
///
/// Time/depth should be understood as follows: If a fact depend on some fact
/// which has time/depth `k` then this facts has at most time/depth `k + 1`.
///
 mod Fixpoint3.Phase.Provenance {
    use Fixpoint3.Ast.Ram
    use Fixpoint3.Ast.Ram.{RamProgram, RamStmt, RelOp, RamTerm, RelSym, arityOf, RowVar, RamId, BoolExp}
    use Fixpoint3.Boxed
    use Fixpoint3.Boxed.BoxedInt64
    use Fixpoint3.Counter

    ///
    /// If `withProv` is `False`: `program` is returned unchanged.
    ///
    /// If `withProv` is `True`: Augment `program` to maintain proof depth of atoms and
    /// rule used to generate them. Further add extend facts to contain a depth and rule.
    ///
    @Internal
    pub def augmentProgram(withProv: Bool, program: RamProgram): RamProgram =
        if (not withProv) program
        else augmentProgramInternal(program)

    ///
    /// Augment `program` to maintain proof depth of atoms and rule used to generate them.
    /// Further extend facts to contain a depth and rule.
    ///
    def augmentProgramInternal(program: RamProgram): RamProgram = match program {
        case RamProgram.Program(stmt, facts, predicates, indexInfo) => unsafely IO run {
            let (f1, f2) = facts;
            let augmentedF1 = f1 |> Map.map(augmentFacts);
            let augmentedF2 = f2 |> Map.map(augmentFacts);
            let augmentedFacts = (augmentedF1, augmentedF2);
            let augmentedStmt = augmentStmt(stmt);
            RamProgram.Program(augmentedStmt, augmentedFacts, predicates, indexInfo) |>
                Fixpoint3.Debugging.notifyPreLowering("Provenance")
        }
    }

    ///
    /// All facts in `oldTree` are expanded/annotated to include a depth and rule number.
    ///
    /// By convention the order of augmentation is depth then rule. Facts have depth 0
    /// and by convention rule -1.
    ///
    /// A fact of the form `[42i64, "someString", Nil]` will be augmented to
    /// `[42i64, "someString", Nil, 0, -1]`.
    ///
    def augmentFacts(oldTree: BPlusTree[Vector[Boxed], Boxed, Static]): BPlusTree[Vector[Boxed], Boxed, Static] \ IO = {
        let newTree = BPlusTree.empty(Static);
        BPlusTree.forEach(tuple -> v ->
            let annotatedVector = tuple ++ Vector#{BoxedInt64(0i64), BoxedInt64(-1i64)};
            BPlusTree.put(annotatedVector, v, newTree)
        , oldTree);
        newTree
    }

    ///
    /// Augments `RelOp`'s in `stmt` with `ProvMax` terms.
    ///
    def augmentStmt(stmt: RamStmt): RamStmt \ r= match stmt {
        case RamStmt.Insert(op) => augmentOp(Nil, op) |> RamStmt.Insert
        case RamStmt.MergeInto(_, _) => stmt
        case RamStmt.Swap(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Seq(stmts) => Vector.map(augmentStmt, stmts) |> RamStmt.Seq
        case RamStmt.Par(stmts) => Vector.map(augmentStmt, stmts) |> RamStmt.Par
        case RamStmt.Until(bools, rest) => RamStmt.Until(bools, augmentStmt(rest))
        case RamStmt.Comment(_) => stmt
    }

    ///
    /// Augments `op` with `ProvMax` terms.
    ///
    def augmentOp(rowVarsArity: List[(RowVar, Int32)], op: RelOp): RelOp \ r = match op {
        case RelOp.Search(rowVar, relSym, body) =>
            augmentOp((rowVar, arityOf(relSym)) :: rowVarsArity, body) |>
                RelOp.Search(rowVar, relSym)
        case RelOp.Query(rowVar, relSym, bools, index, body) =>
            augmentOp((rowVar, arityOf(relSym)) :: rowVarsArity, body) |>
                RelOp.Query(rowVar, relSym, bools, index)
        case RelOp.Functional(_, _, _, _, _) =>
            bug!("In Fixpoint.Phase.Provenance.augmentOp: Cannot do provenance with functional")
        case RelOp.Project(terms, relSym, ruleNum) =>
            let newTerms = terms ++ Vector#{
                RamTerm.ProvMax(List.toVector(rowVarsArity)),
                RamTerm.Lit(BoxedInt64(Int32.toInt64(ruleNum)), RamId.Id(-1))
            };
            RelOp.Project(newTerms, relSym, ruleNum)
        case RelOp.If(boolExps, body) =>
            augmentOp(rowVarsArity, body) |> RelOp.If(boolExps)
    }
}