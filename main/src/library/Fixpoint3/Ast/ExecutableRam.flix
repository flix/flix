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
/// Purpose of `ExecutableRam`: Low level representation of the program to be executed.
/// This representation is tightly coupled to the current interpreter e.g. `Ram`'s `NotBot`
/// is removed since the interpreter handles it explicitly.
///

mod Fixpoint.Ast.ExecutableRam {
    use Fixpoint3.Ast.Ram.{Predicates, RelSym, Search, IndexInformation}
    use Fixpoint.Ast.Shared.{PredSym, BoxedDenotation => Denotation}
    use Fixpoint3.Boxed;
    use Fixpoint3.BoxingType.{UnifiedTypePos, RamIdToPos};
    use Fixpoint.SubstitutePredSym

    /////////////////////////////////////////////////////////////////////////////
    // RamProgram                                                              //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// A `RamProgram` represents a further compiled Datalog program ready to be run
    /// by the interpreter.
    /// 
    pub enum RamProgram {
        case Program(RamStmt, Facts, Predicates, IndexInformation, (Arities, ConstWrites), RamIdToPos)
    }
    ///
    /// The EDB of a program.
    ///
    @Internal
    pub type alias Facts = Map[RelSym, Vector[(Vector[Int64], Boxed)]]

    ///
    /// `ConstWrites` describes constants used for queries and the position they should be
    /// written to. Explicitly for `(id1, id2, val)` we have that
    /// `searchEnv[id1][id2] = val`.
    ///
    /// They come from rules with constants like `Head(x):- Body(4, x)`. We would here
    /// a `ConstWrite` of `Vector#{(idOfBody, 0, 4)}`.
    ///
    @Internal
    pub type alias ConstWrites = Vector[(Int32, Int32, Int64)]

    ///
    /// `WriteTuple` describes where constants for a context-specified RowVar should be
    /// written to. Explicitly for `(indexInFrom, indexTo, indexInTo)` we have that for
    /// the context-specified `RowVar`, `rv`, write
    /// `searchEnv[indexTo][indexInTo] = rv[indexInFrom]`.
    ///
    /// They come from rules with constants like `Head(x):- Body1(x), Body2(y, x)`. We would here
    /// a `WriteTuple` of `Vector#{(0, idOfBody2, 1)}`.
    ///
    @Internal
    pub type alias WriteTuple = (Int32, Int32, Int32)

    ///
    /// `Arities` describes the arity-information for a program.
    ///
    /// For e.g. `Head(x):- Body(x, _, _)`. Assuming `Head` gets position `0` and `Body` gets position `1`
    /// We would here have `Vector#{1, 3}`.
    ///
    type alias Arities = Vector[Int32]

    /////////////////////////////////////////////////////////////////////////////
    // RamStmt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// `Insert(body)` executes the RelOp `body`.
    ///
    /// `MergeInto(posFrom, posTo, leq, isLatticenal)` merges the facts of the index at `posFrom`
    /// into `posTo`. `isLatticenal` is describes whether lattice semantics should be used
    /// and if so `leq` determines how to settle conflicts.
    ///
    /// `Swap(newPos, deltaPos)` swaps the facts of the indexes placed at `newPos` and
    /// `deltaPos`.
    ///
    /// `Purge(pos)` removes all facts from the index at position `pos`.
    ///
    /// `Seq(body)` is sequence of statements to be executed sequentially.
    ///
    /// `Par(body)` is a sequence of statments to be executed in parallel.
    ///
    /// `Until(condition, body)` checks if `condition` is false and repeadetly executes
    /// `body` until `condition` is `true`.
    ///
    /// `Comment(string)` is a comment for debugging.
    ///
    /// `EstimateJoinSize(indexPosition, savePosition, search)` estimates the size of the
    /// index at `indexPosition` as described by `search` and save the result to
    /// `savePosition`. For details see `JoinOptimizer.flix`.
    ///
    @Internal
    pub enum RamStmt {
        case Insert(RelOp)
        case MergeInto(Int32, Int32, Boxed -> Boxed -> Boxed, Bool)
        case Swap(Int32, Int32)
        case Purge(Int32)
        case Seq(Vector[RamStmt])
        case Par(Vector[RamStmt])
        case Until(Vector[BoolExp], RamStmt)
        case Comment(String)
        case EstimateJoinSize(Int32, Int32, Vector[Int32])
    }

    instance ToString[RamStmt] {
        pub def toString(stmt: RamStmt): String =
            let nl = String.lineSeparator();
            match stmt {
                case RamStmt.Insert(op) => ToString.toString(op)
                case RamStmt.MergeInto(src, dst, _, _) => "Merge indexes[${src}] into indexes[${dst}]"
                case RamStmt.Swap(lhs, rhs) => "Swap indexes[${lhs}] and indexes[${rhs}]"
                case RamStmt.Purge(ramSym) => "Purge indexes[${ramSym}]"
                case RamStmt.Seq(xs) => Vector.join(";${nl}", xs)
                case RamStmt.Until(test, body) =>
                    let tst = test |> Vector.join(" ∧ ");
                    "Until(${tst}) do${nl}${String.indent(4, "${body}")}end"
                case RamStmt.Comment(comment) => "/* ${comment} */"
                case RamStmt.Par(xs) => Vector.join("|${nl}", xs)
                case RamStmt.EstimateJoinSize(index, writeTo, attr) => 
                    "EstimateJoinSize(<optimized out>, {${Vector.join(", ", attr)}}) @ ${index}, ${writeTo}"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RelOp                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    
    ///
    /// `Search(rvPos, indexPos, meet, den, writeTuple, body)` iterates the facts in the
    /// index at `indexPos` saving them at `rvPos` and using the facts as context for
    /// `writeTuple`.
    ///
    /// If `den` is a relational execute `body`.
    ///
    /// If `den` is a latticenal, saves the lattice element at `rvPos`. If
    /// `meet != rvPos`, saves the meet of the lattice element and the element at `meet`.
    /// If the value saved to `rvPos` is not `Bot` execute `body`.
    ///
    /// `Query(rvPos, indexPos, meet, den, writeTuple, body)` is the same as `Search`,
    /// except the facts will be filtered by the query for `rvPos`.
    ///
    /// `Functional(rv, f, input, writeTuple, body, unboxingInfo)` evaluates `f(input)`,
    /// for each result saves it as `rv`, does writes as described by `writeTuple` and
    /// executes `body`. `unboxingInfo` can be used to unbox the values returned by `f`.
    ///
    /// `Project(terms, newIndex, den)` constructs a tuple from `terms` and inserts it a
    /// fact in index `newIndex`. For `den == Latticenal` we only insert if the
    /// lattice-value is greater than the current and we insert with least uppber bound.
    ///
    /// `If(bools, body)` evaluates `body` if `bools` is true.
    ///
    @Internal
    pub enum RelOp {
        case Search(Int32, Int32, Int32, Denotation, Vector[WriteTuple], RelOp)
        case Query(Int32, Int32, Int32, Denotation, Vector[WriteTuple], RelOp)
        case Functional(Int32, Vector[Boxed] -> Vector[Vector[Boxed]], Vector[RamTerm], Vector[WriteTuple], RelOp, Vector[UnifiedTypePos])
        case Project(Vector[RamTerm], Int32, Denotation)
        case If(Vector[BoolExp], RelOp)
    }

    instance ToString[RelOp] {
        pub def toString(op: RelOp): String =
            let nl = String.lineSeparator();
            match op {
                case RelOp.Search(var, relSym, _, _, writes, body) =>
                    let writeString = Vector.joinWith(match term -> ToString.toString(term), " ∧ ", writes);
                    "search x${var} ∈ indexes[${relSym}] | ${writeString} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Query(var, ramSym, _, _, writes, body) =>
                    let writeString = Vector.joinWith(match term -> ToString.toString(term), " ∧ ", writes);
                    "query {x${var} ∈ indexes[${ramSym}] | ${writeString}} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Functional(id, _, terms, _, body, _) => 
                    "loop(x${id} <- f(${terms |> Vector.join(", ")})) do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Project(terms, ramSym, _) =>
                    "project (${terms |> Vector.join(", ")}) into ${ramSym}"
                case RelOp.If(test, then) =>
                    let tst = test |> Vector.join(" ∧ ");
                    "if(${tst}) then${nl}${String.indent(4, "${then}")}end"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // BoolExp                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// `Not(b)`: True if `b` is false.
    ///
    /// `IsEmpty(index)`: True if there are no facts in the index at `index`.
    ///
    /// `NotMemberOf(terms, index, den)`: True if `terms` is not a fact in the facts
    /// placed at `index`.
    ///
    /// `Leq(const, pos)`: True if `const` is less than the lattice element of `pos`.
    ///
    /// `Eq(t1, t2)`: True if `t1 == t2`.
    ///
    /// `Guard1(f, term1)`: True if `f(term1) == true`.
    ///
    /// `Guard2(f, term1, term2)`: True if `f(term1, term2) == true`.
    ///
    /// `Guard3(f, term1, term2, term3)`: True if `f(term1, term2, term3) == true`.
    ///
    /// `Guard4(f, term1, term2, term3, term4)`: True if `f(term1, term2, term3, term4) == true`.
    ///
    /// `Guard5(f, term1, term2, term3, term4, term5)`: True if `f(term1, term2, term3, term4, term5) == true`.
    ///
    @Internal
    pub enum BoolExp {
        case Not(BoolExp)
        case IsEmpty(Int32)
        case NotMemberOf(Vector[RamTerm], Int32, Denotation)
        case Leq(Boxed, Boxed -> Boxed -> Bool, Int32)
        case Eq(RamTerm, RamTerm)
        case Guard1(Boxed -> Bool, RamTerm)
        case Guard2(Boxed -> Boxed  -> Bool, RamTerm, RamTerm)
        case Guard3(Boxed -> Boxed  -> Boxed -> Bool, RamTerm, RamTerm, RamTerm)
        case Guard4(Boxed -> Boxed  -> Boxed -> Boxed -> Bool, RamTerm, RamTerm, RamTerm, RamTerm)
        case Guard5(Boxed -> Boxed  -> Boxed -> Boxed -> Boxed -> Bool, RamTerm, RamTerm, RamTerm, RamTerm, RamTerm)
    }

    instance ToString[BoolExp] {
        pub def toString(exp: BoolExp): String =
            match exp {
                case BoolExp.Not(boolExp) => "not (${boolExp})"
                case BoolExp.IsEmpty(relSym) => "indexes[${relSym}] = ∅"
                case BoolExp.NotMemberOf(terms, relSym, _) => "(${terms |> Vector.join(", ")}) ∉ indexes[${relSym}]"
                case BoolExp.Eq(lhs, rhs) => "${lhs} = ${rhs}"
                case BoolExp.Leq(lhs, _, rhs) => "${lhs} ≤ (${rhs})"
                case BoolExp.Guard1(_, v) => "<clo>(${v})"
                case BoolExp.Guard2(_, v1, v2) => "<clo>(${v1}, ${v2})"
                case BoolExp.Guard3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
                case BoolExp.Guard4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case BoolExp.Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RamTerm                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Represents a RAM term.
    ///
    /// `Lit(rep, val)` represents the literal `val` and its unboxed version, `rep`.
    ///
    /// `ProvMax([(memPos1, i1), (memPos2, i2), ...])` represents 
    /// `max(memory[memPos1][i1], memory[memPos2][i2], ...)`
    ///
    /// `LoadFromTuple(memPos, i, unboxingInfo)` represents an index from the tuple stored at postion
    /// `memPos`, i.e. `memory[memPos][i]`. Can be boxed using `unboxingInfo`.
    ///
    /// `LoadLatVar(memPos, unboxingInfo)` represents loading the lattice element from the tuple stored
    /// at `memPos`. Can be unboxed using `unboxingInfo`.
    ///
    /// `Meet(meet, term1, term2, unboxingInfo)` represents the meet of `term1` with `term2`
    /// Can be unboxed using `unboxingInfo`.
    ///
    /// `App1(f, term1, )` represents `f(term1)`.
    /// Can be unboxed using `unboxingInfo`.
    ///
    /// `App2(f, term1, term2)` represents `f(term1, term2)`.
    /// Can be unboxed using `unboxingInfo`.
    ///
    /// `App3(f, term1, term2, term3)` represents `f(term1, term2, term3)`.
    /// Can be unboxed using `unboxingInfo`.
    ///
    /// `App4(f, term1, term2, term3, term4)` represents `f(term1, term2, term3, term4)`.
    /// Can be unboxed using `unboxingInfo`.
    ///
    /// `App5(f, term1, term2, term3, term4, term5)` represents `f(term1, term2, term3, term4, term4)`.
    /// Can be unboxed using `unboxingInfo`.
    ///
    @Internal
    pub enum RamTerm {
        case Lit(Int64, Boxed)
        case ProvMax(Vector[(Int32, Int32)])
        case LoadFromTuple(Int32, Int32, UnifiedTypePos)
        case LoadLatVar(Int32, UnifiedTypePos)
        case Meet(Boxed -> Boxed  -> Boxed, RamTerm, RamTerm, UnifiedTypePos)
        case App1(Boxed -> Boxed, RamTerm, UnifiedTypePos)
        case App2(Boxed -> Boxed  -> Boxed, RamTerm, RamTerm, UnifiedTypePos)
        case App3(Boxed -> Boxed  -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, UnifiedTypePos)
        case App4(Boxed -> Boxed  -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, UnifiedTypePos)
        case App5(Boxed -> Boxed  -> Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, RamTerm, UnifiedTypePos)
    }

    instance ToString[RamTerm] {
        pub def toString(term: RamTerm): String = match term {
            case RamTerm.Lit(v, _) => "%{v}"
            case RamTerm.LoadFromTuple(tupleIndex, indexInTuple, _) => "x${tupleIndex}[${indexInTuple}]"
            case RamTerm.LoadLatVar(i, _) => "LoadLatVar[${i}]"
            case RamTerm.ProvMax(loads) =>
                let loadsStr = Vector.joinWith(match (rv, i) -> "${rv}[${i}]", ", ", loads);
                "max(${loadsStr})"
            case RamTerm.Meet(_, lhs, rhs, _) => "(${lhs} ⊓ ${rhs})"
            case RamTerm.App1(_, v, _) => "<clo>(${v})"
            case RamTerm.App2(_, v1, v2, _) => "<clo>(${v1}, ${v2})"
            case RamTerm.App3(_, v1, v2, v3, _) => "<clo>(${v1}, ${v2}, ${v3})"
            case RamTerm.App4(_, v1, v2, v3, v4, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case RamTerm.App5(_, v1, v2, v3, v4, v5, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }
    
}
