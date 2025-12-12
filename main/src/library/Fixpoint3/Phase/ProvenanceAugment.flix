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
/// This module removes `BoolExp.NotMemberOf` as the `ProvProject` introduced by `Lowering`
/// must ensure this property. Note that only this does not refers to the `NotMemberOf` relating
/// to negative dependencies/the `not A(...)` constructs.
///
 mod Fixpoint3.Phase.ProvenanceAugment {
    use Fixpoint3.Ast.Ram
    use Fixpoint3.Ast.Ram.{RamProgram, RamStmt, RelOp, RamTerm, RelSym, arityOf, RowVar, BoolExp}
    use Fixpoint3.Boxed
    use Fixpoint3.Boxed.BoxedInt64
    use Fixpoint3.Counter
    use Fixpoint3.Options.usedArity
    use Fixpoint3.TypeInfo.{expandTypeToProv, expandTypeInfoToProv, provType}

    ///
    /// If `withProv` is `False`: `program` is returned unchanged.
    ///
    /// If `withProv` is `True`: Augment `program` to maintain proof depth of atoms and
    /// rule used to generate them. Further add extend facts to contain a depth and rule.
    ///
    pub def augmentProgram(withProv: Bool, program: RamProgram): RamProgram =
        if (not withProv) program
        else augmentProgramInternal(program)

    ///
    /// Augment `program` to maintain proof depth of atoms and rule used to generate them.
    /// Further extend facts to contain a depth and rule.
    ///
    def augmentProgramInternal(program: RamProgram): RamProgram = match program {
        case RamProgram.Program(stmt, facts, predicates, indexInfo, typeInfo) => unsafe IO {
            let (f1, f2) = facts;
            let augmentedF1 = Map.map(augmentFacts, f1);
            let augmentedF2 = Map.map(augmentFacts, f2);
            let augmentedFacts = (augmentedF1, augmentedF2);
            let augmentedStmt = augmentStmt(stmt);
            let newTypeInfo = expandTypeInfoToProv(typeInfo, predicates);
            RamProgram.Program(augmentedStmt, augmentedFacts, predicates, indexInfo, newTypeInfo) |>
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
        let newTree = BPlusTree.emptyWithArity(Static, usedArity());
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
        case RamStmt.Insert(op)         => RamStmt.Insert(augmentOp(Nil, op))
        case RamStmt.MergeInto(_, _)    => stmt
        case RamStmt.Swap(_, _)         => stmt
        case RamStmt.Purge(_)           => stmt
        case RamStmt.Seq(stmts)         => RamStmt.Seq(Vector.map(augmentStmt, stmts))
        case RamStmt.Par(stmts)         => RamStmt.Par(Vector.map(augmentStmt, stmts))
        case RamStmt.Until(bools, rest) => RamStmt.Until(removeNotMemberOf(bools), augmentStmt(rest))
        case RamStmt.Comment(_)         => stmt
    }

    ///
    /// Augments `op` with `ProvMax` terms.
    ///
    def augmentOp(rowVarsArity: List[(RowVar, Int32)], op: RelOp): RelOp \ r = match op {
        case RelOp.Search(rowVar, relSym, t, body) =>
            augmentOp((rowVar, arityOf(relSym)) :: rowVarsArity, body) |>
                RelOp.Search(rowVar, relSym, t)
        case RelOp.Query(rowVar, relSym, bools, index, t, body) =>
            augmentOp((rowVar, arityOf(relSym)) :: rowVarsArity, body) |>
                RelOp.Query(rowVar, relSym, removeNotMemberOf(bools), index, t)
        case RelOp.Functional(rowVar, func, terms, body, arity, t) =>
            // Do not add this to `rowVarsArity`. We use this in `ProvMax` as `Functional`
            // has no concept of depth.
            RelOp.Functional(rowVar, func, terms, augmentOp(rowVarsArity, body), arity, t)
        case RelOp.Project(terms, relSym, ruleNum) =>
            let newTerms = terms ++ Vector#{
                RamTerm.ProvMax(List.toVector(rowVarsArity)),
                RamTerm.Lit(BoxedInt64(Int32.toInt64(ruleNum)), provType())
            };
            RelOp.Project(newTerms, relSym, ruleNum)
        case RelOp.If(boolExps, body) =>
            RelOp.If(removeNotMemberOf(boolExps), augmentOp(rowVarsArity, body))
    }


    ///
    /// Returns the `BoolExp`'s in `boolExps` without `NotMemberOf`. The `NotMemberOf` introduced by
    /// negative dependencies are kept (`not A(...)`).
    ///
    def removeNotMemberOf(boolExps: Vector[BoolExp]): Vector[BoolExp] = Vector.filterMap(notNotMemberOf, boolExps)

    ///
    /// Returns `Some(boolExp)` if `boolExp` is not `NotMemberOf`.
    ///
    /// Otherwise if `boolExp` is `NotMemberOf` it returns `Some(boolExp)` if the expression was
    /// introduced by a negative dependency (`not A(...)`).
    ///
    def notNotMemberOf(boolExp: BoolExp): Option[BoolExp] = match boolExp {
        case BoolExp.NotMemberOf(_, _, false) => None
        case _ => Some(boolExp)
    }

}
