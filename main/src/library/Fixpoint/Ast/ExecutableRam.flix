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

mod Fixpoint.Ast.ExecutableRam {
    use Fixpoint.Boxed;
    use Fixpoint.Ast.Ram.{PredicateState, RelSym, Search, IndexInformation}
    use Fixpoint.Ast.Shared.{PredSym, Denotation}
    use Fixpoint.SubstitutePredSym

    /////////////////////////////////////////////////////////////////////////////
    // RamProgram                                                              //
    /////////////////////////////////////////////////////////////////////////////

    // RamProgram consisting of the the ExecutableRam program/statements, 
    // the starting database/facts and information about which indices should be used.
    pub enum RamProgram {
        case Program(RamStmt, Facts, PredicateState, IndexInformation, (Arities, ConstWrites), Fixpoint.Boxing.RamIdToMarshal)
    }

    pub type alias Facts = Map[RelSym, Vector[(Vector[Int64], Boxed)]]
    // (id1, id2, val): minEnv[id1][id2] = val and maxEnv[id1][id2] = val
    pub type alias ConstWrites = Vector[(Int32, Int32, Int64)]
    // (a, b, c) meaning: Write number a from the read values to search environment number b at position c.
    // i.e. searchEnv[b][c] = read[a]
    pub type alias WriteTuple = (Int32, Int32, Int32)
    type alias IntToBox = Int32
    type alias Arities = Vector[Int32]

    /////////////////////////////////////////////////////////////////////////////
    // RamStmt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum RamStmt {
        case Insert(RelOp)
        // Boxed -> Boxed -> Boxed and IntToBox here only means something for lattice-tuples.
        case Merge(Int32, Int32, Boxed -> Boxed -> Boxed, IntToBox)
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
                case RamStmt.Merge(src, dst, _, _) => "merge indexes[${src}] into indexes[${dst}]"
                case RamStmt.Swap(lhs, rhs) => "Swap indexes[${lhs}] and indexes[${rhs}]"
                case RamStmt.Purge(ramSym) => "purge indexes[${ramSym}]"
                case RamStmt.Seq(xs) => Vector.join(";${nl}", xs)
                case RamStmt.Until(test, body) =>
                    let tst = test |> Vector.join(" ∧ ");
                    "until(${tst}) do${nl}${String.indent(4, "${body}")}end"
                case RamStmt.Comment(comment) => "// ${comment}"
                case RamStmt.Par(xs) => Vector.join("|${nl}", xs)
                case RamStmt.EstimateJoinSize(index, writeTo, attr) => 
                    "EstimateJoinSize(<optimized out>, {${Vector.join(", ", attr)}}) @ ${index}, ${writeTo}"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RelOp                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    
    @Internal
    pub enum RelOp {
        // (RowVar, searchIndex, writeTuple, body)
        case Search(Int32, Int32, Vector[WriteTuple], RelOp)
        // (RowVar, searchIndex, meetWith, meet, writeTuple, body)
        case SearchLattice(Int32, Int32, Int32, Denotation, Vector[WriteTuple], RelOp)
        // (RowVar: EnvPos, searchIndex, body)
        case Query(Int32, Int32, Vector[WriteTuple], RelOp)
        // (RowVar: EnvPos, searchIndex, meetWith, Denotation, writeLatticeTo, body)
        case QueryLattice(Int32, Int32, Int32, Denotation, Vector[WriteTuple], RelOp)
        // The first is from the second is to.
        case Functional(Int32, Vector[Boxed] -> Vector[Vector[Boxed]], Vector[RamTerm], Vector[WriteTuple], RelOp, Vector[IntToBox])
        // The Boxed is Bot. The first Boxed -> Boxed -> Boxed is leq and the second is lub.
        case Project(Vector[RamTerm], Int32, Boxed, Boxed -> Boxed -> Bool, Boxed -> Boxed -> Boxed, IntToBox)
        case If(Vector[BoolExp], RelOp)
    }

    instance ToString[RelOp] {
        pub def toString(op: RelOp): String =
            let nl = String.lineSeparator();
            match op {
                case RelOp.Search(var, relSym, writes, body) =>
                    let writeString = Vector.joinWith(match term -> ToString.toString(term), " ∧ ", writes);
                    "search x${var} ∈ indexes[${relSym}] | ${writeString} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Query(var, ramSym, writes, body) =>
                    let writeString = Vector.joinWith(match term -> ToString.toString(term), " ∧ ", writes);
                    "query {x${var} ∈ indexes[${ramSym}] | ${writeString}} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.SearchLattice(var, relSym, _, _, writes, body) =>
                    let writeString = Vector.joinWith(match term -> ToString.toString(term), " ∧ ", writes);
                    "searchLat x${var} ∈ indexes[${relSym}] | ${writeString} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.QueryLattice(var, ramSym, _, _, writes, body) =>
                    let qry = Vector.joinWith(match term -> ToString.toString(term), " ∧ ", writes);
                    "queryLat {x${var} ∈ indexes[${ramSym}] | ${qry}} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Functional(id, _, terms, _, body, _) => 
                    "loop(x${id} <- f(${terms |> Vector.join(", ")})) do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Project(terms, ramSym, _, _, _, _) =>
                    "project (${terms |> Vector.join(", ")}) into ${ramSym}"
                case RelOp.If(test, then) =>
                    let tst = test |> Vector.join(" ∧ ");
                    "if(${tst}) then${nl}${String.indent(4, "${then}")}end"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // BoolExp                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum BoolExp {
        case Not(BoolExp)
        case IsEmpty(Int32)
        // Boxed -> Boxed -> bool is leq.
        case NotMemberOf(Vector[RamTerm], Int32, Boxed -> Boxed -> Bool, IntToBox)
        case Eq(RamTerm, RamTerm)
        case Leq(Boxed, Boxed -> Boxed -> Bool, Int32)
        case Guard0(Unit -> Bool)
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
                case BoolExp.NotMemberOf(terms, relSym, _, _) => "(${terms |> Vector.join(", ")}) ∉ indexes[${relSym}]"
                case BoolExp.Eq(lhs, rhs) => "${lhs} = ${rhs}"
                case BoolExp.Leq(lhs, _, rhs) => "${lhs} ≤ (${rhs})"
                case BoolExp.Guard0(_) => "<clo>()"
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
    /// `LoadFromTuple(x, i, id)` represents an index into the tuple
    /// bound to the local variable `x`, i.e. `x[i]`. If `x[i]` is a
    /// boxed term, `id` is the position of its type information. 
    ///
    /// `LoadLatVar(x, id)` represents the lattice component in the tuple
    /// bound to the local variable `x`.
    ///
    @Internal
    pub enum RamTerm {
        case Lit(Int64, Boxed)
        case LoadFromTuple(Int32, Int32, IntToBox)
        case LoadLatVar(Int32, IntToBox)
        case Meet(Boxed -> Boxed  -> Boxed, RamTerm, RamTerm, IntToBox)
        // The first indices are the input. The last is the output
        case App0(Unit -> Boxed, IntToBox)
        case App1(Boxed -> Boxed, RamTerm, IntToBox)
        case App2(Boxed -> Boxed  -> Boxed, RamTerm, RamTerm, IntToBox)
        case App3(Boxed -> Boxed  -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, IntToBox)
        case App4(Boxed -> Boxed  -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, IntToBox)
        case App5(Boxed -> Boxed  -> Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, RamTerm, IntToBox)
    }

    instance ToString[RamTerm] {
        pub def toString(term: RamTerm): String = match term {
            case RamTerm.Lit(v, _) => "%{v}"
            case RamTerm.LoadFromTuple(tupleIndex, indexInTuple, _) => "x${tupleIndex}[${indexInTuple}]"
            case RamTerm.LoadLatVar(i, _) => "LoadLatVar[${i}]"
            case RamTerm.Meet(_, lhs, rhs, _) => "(${lhs} ⊓ ${rhs})"
            case RamTerm.App0(_, _) => "<clo>()"
            case RamTerm.App1(_, v, _) => "<clo>(${v})"
            case RamTerm.App2(_, v1, v2, _) => "<clo>(${v1}, ${v2})"
            case RamTerm.App3(_, v1, v2, v3, _) => "<clo>(${v1}, ${v2}, ${v3})"
            case RamTerm.App4(_, v1, v2, v3, v4, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case RamTerm.App5(_, v1, v2, v3, v4, v5, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }
    
}
