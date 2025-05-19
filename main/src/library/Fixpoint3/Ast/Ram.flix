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

mod Fixpoint3.Ast.Ram {
    use Fixpoint.Ast.Shared.{BoxedDenotation => Denotation, PredSym}
    use Fixpoint.Ast.Shared.Denotation.{Relational}
    use Fixpoint3.Boxed
    use Fixpoint.PredSymsOf
    use Fixpoint.SubstitutePredSym

    /////////////////////////////////////////////////////////////////////////////
    // RamProgram                                                              //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// A `RamProgram` represents a compiled Datalog program.
    /// 
    @Internal
    pub enum RamProgram {
        case Program(RamStmt, Facts, Predicates, IndexInformation)
    }

    ///
    /// Contains a set of `RelSym` representing the names of the relations in the program
    ///  and 1 + the maximal identifier of the `RelSym`s.
    ///
    @Internal
    pub type alias Predicates = (Int64, Set[RelSym])

    ///
    /// The EDB of a program.
    ///
    @Internal
    pub type alias Facts = Map[RelSym, Vector[Vector[Boxed]]]

    ///
    /// A `Search` describes the order in which entries in a tuple are ordered. These are
    /// created during the index selection phase and are used by indexes to store tuples
    /// for efficient retrieval in range queries.
    ///
    /// For example, the Datalog rule
    /// 
    /// A(x) :- B(x), C(_, _, x).
    ///
    /// produces the search `Vector#{2, 0, 1}` for relation `C`. This means the tuples in
    /// `C` are ordered by the third element, then the first and finally the second.
    /// During the index selection phase, searches are collected and are expanded to
    /// describe the order of the whole tuple, i.e., the above search is collected as
    /// `Vector#{2}` but gets expanded to `Vector#{2, 0, 1}`. Thus, the final search used
    /// by an index has length exactly equal to the relation's arity.
    ///
    @Internal
    pub type alias Search = Vector[Int32]

    ///
    /// Maps to the set of `Search` for a `RelSym` in the Datalog program. It represents
    /// the indexes that will be created for `RelSym` during program evaluation.
    ///
    @Internal
    pub type alias Indexes = Map[RelSym, Vector[Search]]

    ///
    /// Maps a `RelSym` and its place in `Indexes[RelSym]` to its final position in memory.
    ///
    @Internal
    pub type alias IndexPositions = Map[(RelSym, Int32), Int32]

    ///
    /// The indexes to be created during program evaluation and their positions in memory.
    ///
    @Internal
    pub type alias IndexInformation = (Indexes, IndexPositions)

    /////////////////////////////////////////////////////////////////////////////
    // RamStmt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// `Insert(body)` executes the RelOp `body`.
    ///
    /// `MergeInto(r1, r2)` merges the facts of `r1` into `r2`.
    ///
    /// `Swap(newRel, deltaRel)` swaps the facts of `deltaRel` and `newRel`. For
    /// multi-indexed relations, the only guarantee is that all indexes on `deltaRel` will
    /// contain all facts of `newRel`.
    ///
    /// `Purge(rel)` removes all facts from all indexes on `rel`.
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
    /// `EstimateJoinSize(relSym, indexUsed, savePosition, search)` estimates the size
    /// relation `relSym` based on index `indexUsed` as described by `search` and saves
    /// the result to `savePosition`. For details see JoinOptimizer.
    ///
    @Internal
    pub enum RamStmt {
        case Insert(RelOp)
        case MergeInto(RelSym, RelSym)
        case Swap(RelSym, RelSym)
        case Purge(RelSym)
        case Seq(Vector[RamStmt])
        case Par(Vector[RamStmt])
        case Until(Vector[BoolExp], RamStmt)
        case Comment(String)
        case EstimateJoinSize(RelSym, Int32, Int32, Search)
    }

    instance ToString[RamStmt] {
        pub def toString(stmt: RamStmt): String =
            let nl = String.lineSeparator();
            match stmt {
                case RamStmt.Insert(op) => ToString.toString(op)
                case RamStmt.MergeInto(src, dst) => "Merge ${src} into ${dst}"
                case RamStmt.Swap(lhs, rhs) => "Swap ${lhs} and ${rhs}"
                case RamStmt.Purge(relSym) => "Purge ${relSym}"
                case RamStmt.Seq(xs) => Vector.join(";${nl}", xs)
                case RamStmt.Until(test, body) =>
                    let tst = test |> Vector.join(" && ");
                    "until(${tst}) do${nl}${String.indent(4, "${body}")}end"
                case RamStmt.Comment(comment) => "/* ${comment} */"
                case RamStmt.Par(xs) => Vector.join("|${nl}", xs)
                case RamStmt.EstimateJoinSize(relSym, _, _, attr) =>
                    "EstimateJoinSize(${relSym}, {${Vector.join(", ", attr)}})"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RelOp                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    
    ///
    /// `Search(rv, relSym, body)` iterates through the facts in relation `relSym`, saving
    /// them as RowVar `rv` and executes `body`.
    ///
    /// `Query(rv, relSym, bools, indexPos, body)` iterates through the facts in relation
    /// `relSym`, saving them as `rv` and executes `body` if `bools` is
    /// true. The index saved at `indexPos` will be used.
    ///
    /// `Functional(rv, f, input, body, arity)` evaluates `f(input)`, for each result,
    /// saves it as `rv` and executes `body`. `arity` is the arity of the output tuples.
    ///
    /// `Project(terms, relSym)` constructs a tuple from `terms` and inserts it as a fact
    /// for relation `relSym`. `relSym` should be a new relation.
    ///
    /// `If(bools, body)` evaluates `body` if `bools` is true.
    ///
    @Internal
    pub enum RelOp {
        case Search(RowVar, RelSym, RelOp)
        case Query(RowVar, RelSym, Vector[BoolExp], Int32, RelOp)
        case Functional(RowVar, Vector[Boxed] -> Vector[Vector[Boxed]], Vector[RamTerm], RelOp, Int32)
        case Project(Vector[RamTerm], RelSym)
        case If(Vector[BoolExp], RelOp)
    }

    instance ToString[RelOp] {
        pub def toString(op: RelOp): String =
            let nl = String.lineSeparator();
            match op {
                case RelOp.Search(var, relSym, body) =>
                    "search ${var} ∈ ${relSym} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Query(var, relSym, prefixQuery, _, body) =>
                    let qry = Vector.joinWith(match term -> {
                        ToString.toString(term)
                    }, " ∧ ", prefixQuery);
                    "query {${var} ∈ ${relSym} | ${qry}} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Functional(RowVar.Named(id), _, terms, body, _) =>
                    "loop(x${id} <- f(${terms |> Vector.join(", ")})) do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Project(terms, relSym) =>
                    "project (${terms |> Vector.join(", ")}) into ${relSym}"
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
    /// `IsEmpty(relSym)`: True if there are no facts in relation `relSym`.
    ///
    /// `NotMemberOf(terms, relSym)`: True if the tuple constructed from `terms` is not
    /// in relation `relSym`.
    ///
    /// `NotBot(term, leq, bot)`: True if `term` is not a `bot`.
    /// `term` must be a Meet or RowLoad of a lattice var.
    ///
    /// `Leq(const, rv, relSym)`: True if `const` is less than the lattice element of `rv`.
    /// 
    /// `Eq(term1, term2)`: True if `term1 == term2`.
    ///
    /// `Guard1(f, term1)`: True if `f(term1) == true`.
    /// `Guard2(f, term1, term2)`: True if `f(term1, term2) == true`.
    /// `Guard3(f, term1, term2, term3)`: True if `f(term1, term2, term3) == true`.
    /// `Guard4(f, term1, term2, term3, term4)`: True if `f(term1, term2, term3, term4) == true`.
    /// `Guard5(f, term1, term2, term3, term4, term5)`: True if `f(term1, term2, term3, term4, term5) == true`.
    ///
    @Internal
    pub enum BoolExp {
        case Not(BoolExp)
        case IsEmpty(RelSym)
        case NotMemberOf(Vector[RamTerm], RelSym)
        case NotBot(RamTerm, Boxed -> Boxed -> Bool, Boxed)
        case Leq(Boxed, RowVar, RelSym)
        case Eq(RamTerm, RamTerm)
        case Guard1(Boxed -> Bool, RamTerm)
        case Guard2(Boxed -> Boxed -> Bool, RamTerm, RamTerm)
        case Guard3(Boxed -> Boxed -> Boxed -> Bool, RamTerm, RamTerm, RamTerm)
        case Guard4(Boxed -> Boxed -> Boxed -> Boxed -> Bool, RamTerm, RamTerm, RamTerm, RamTerm)
        case Guard5(Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Bool, RamTerm, RamTerm, RamTerm, RamTerm, RamTerm)
    }

    instance ToString[BoolExp] {
        pub def toString(exp: BoolExp): String =
            match exp {
                case BoolExp.Not(boolExp) => "not (${boolExp})"
                case BoolExp.IsEmpty(relSym) => "${relSym} = ∅"
                case BoolExp.NotMemberOf(terms, relSym) => "(${terms |> Vector.join(", ")}) ∉ ${relSym}"
                case BoolExp.NotBot(term, _, _) => "(${term}) ≠ ⊥"
                case BoolExp.Leq(elem, _, term) => "${elem} ≤ (${term})"
                case BoolExp.Eq(lhs, rhs) => "${lhs} = ${rhs}"
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
    /// Enums with a `RamId` can be referred to by the `RamId`.
    ///
    /// `Lit(val, id)` represents the literal `val`.
    ///
    /// `RowLoad(x, i, relSym)` represents an index into the tuple
    /// bound to the local variable `x`, i.e. `x[i]`. `x` is a tuple
    /// from `relSym`.
    ///
    /// `Meet(meet, term, (rv, relSym), id)` represents the meet of `term` with the lattice
    /// element of `rv`.
    ///
    /// `App1(f, term1)` represents `f(term1)`.
    ///
    /// `App2(f, term1, term2)` represents `f(term1, term2)`.
    ///
    /// `App3(f, term1, term2, term3)` represents `f(term1, term2, term3)`.
    ///
    /// `App4(f, term1, term2, term3, term4)` represents `f(term1, term2, term3, term4)`.
    ///
    /// `App5(f, term1, term2, term3, term4, term5)` represents `f(term1, term2, term3, term4, term4)`.
    ///
    @Internal
    pub enum RamTerm {
        case Lit(Boxed, RamId)
        case RowLoad(RowVar, Int32, RelSym)
        case Meet(Boxed -> Boxed -> Boxed, RamTerm, (RowVar, RelSym), RamId)
        case App1(Boxed -> Boxed, RamTerm, RamId)
        case App2(Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamId)
        case App3(Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamId)
        case App4(Boxed -> Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, RamId)
        case App5(Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, RamTerm, RamId)
    }

    ///
    /// Returns the `RamId` of `t`.
    ///
    @Internal
    pub def getTermRamId(t: RamTerm): RamId = match t {
        case RamTerm.Lit(_, id) => id
        case RamTerm.RowLoad(rv, index, _) => RamId.TuplePos(rv, index)
        case RamTerm.Meet(_, _, _, id) => id
        case RamTerm.App1(_ , _, id) => id
        case RamTerm.App2(_, _, _, id) => id
        case RamTerm.App3(_, _, _, _, id) => id
        case RamTerm.App4(_, _, _, _, _, id) => id
        case RamTerm.App5(_, _, _, _, _, _, id) => id
    }

    ///
    /// Returns the `RamId` of the lattice-element of `rv`.
    ///
    @Internal
    pub def getLatVarRamId(rv: RowVar, relSym: RelSym): RamId =
        let arity = arityOfNonLat(relSym);
        RamId.TuplePos(rv, arity)

    instance ToString[RamTerm] {
        pub def toString(term: RamTerm): String = match term {
            case RamTerm.Lit(v, _) => "%{v}"
            case RamTerm.RowLoad(var, index, _) => "${var}[${index}]"
            case RamTerm.Meet(_, lhs, rhs, _) => "(${lhs} ⊓ ${rhs})"
            case RamTerm.App1(_, v, _) => "<clo>(${v})"
            case RamTerm.App2(_, v1, v2, _) => "<clo>(${v1}, ${v2})"
            case RamTerm.App3(_, v1, v2, v3, _) => "<clo>(${v1}, ${v2}, ${v3})"
            case RamTerm.App4(_, v1, v2, v3, v4, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case RamTerm.App5(_, v1, v2, v3, v4, v5, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RelSym                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// `RelSym(predSym, arity, den)` represents the relation of `predSym`
    /// which has arity `arity` and denotation `den`.
    ///
    @Internal
    pub enum RelSym {
        case Symbol(PredSym, Int32, Denotation)
    }

    instance Eq[RelSym] {
        pub def eq(a: RelSym, b: RelSym): Bool = match (a, b) {
            case (RelSym.Symbol(s1, _, _), RelSym.Symbol(s2, _, _)) => s1 == s2
        }
    }

    instance Order[RelSym] {
        pub def compare(a: RelSym, b: RelSym): Comparison = match (a, b) {
            case (RelSym.Symbol(s1, _, _), RelSym.Symbol(s2, _, _)) => s1 <=> s2
        }
    }

    instance PredSymsOf[RelSym] {
        pub def predSymsOf(x: RelSym): Set[PredSym] = match x {
            case RelSym.Symbol(predSym, _, _) => Set.singleton(predSym)
        }
    }

    instance SubstitutePredSym[RelSym] {
        pub def substitute(x: RelSym, s: Map[PredSym, PredSym]): RelSym = match x {
            case RelSym.Symbol(predSym, arity, den) =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                RelSym.Symbol(newSym, arity, den)
        }
    }

    instance ToString[RelSym] {
        pub def toString(relSym: RelSym): String = match relSym {
            case RelSym.Symbol(sym, _, _) => "${sym}"
        }
    }

    ///
    /// Returns the arity of `relSym`.
    ///
    @Internal
    pub def arityOf(relSym: RelSym): Int32 = match relSym {
        case RelSym.Symbol(_, arity, _) => arity
    }

    ///
    /// Returns the arity of the non-lattice part of `relSym`.
    ///
    @Internal
    pub def arityOfNonLat(relSym: RelSym): Int32 = match relSym {
        case RelSym.Symbol(_, arity, den) => match den {
            case Relational => arity
            case _ => arity - 1
        }
    }

    ///
    /// Returns the denotation of `relSym`.
    ///
    @Internal
    pub def toDenotation(relSym: RelSym): Denotation = match relSym {
        case RelSym.Symbol(_, _, den) => den
    }

    ///
    /// Returns the identifier of `relSym`.
    ///
    @Internal
    pub def toId(relSym: RelSym): Int64 = match relSym {
        case RelSym.Symbol(PredSym.PredSym(_, id), _, _) => id
    }

    /////////////////////////////////////////////////////////////////////////////
    // RowVar                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// `Named(id)` represents the unique identifier, `id`, of some row variable
    ///
    ///
    /// For a program:
    /// search e1 ∈ Edge do
    ///    search e2 ∈ Edge do
    ///        ...
    /// `e1` and `e2` would be the identifiers. Identifiers will only be defined
    /// once in a program, but can be reffered to from elsewhere.

    @Internal
    pub enum RowVar with Eq {
        case Named(Int32)
    }

    instance ToString[RowVar] {
        pub def toString(var: RowVar): String = match var {
            case RowVar.Named(id) => "x${id}"
        }
    }

    instance Order[RowVar] {
        pub def compare(a: RowVar, b: RowVar): Comparison = match (a, b) {
            case (RowVar.Named(i1), RowVar.Named(i2)) => i1 <=> i2
        }
    }
    
    /////////////////////////////////////////////////////////////////////////////
    // RamId                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// `RamId` represents a unique identifier for literals, positions in relations,
    /// positions in tuples or input to a function.
    ///
    /// `Id(id)` represents the id `id`. If `Id(id)` exists `RowVar.Named(id)` does not.
    ///
    /// `InId(id, i)` represents the `i`'th input of the term represented by `id`.
    ///
    /// `TuplePos(rv, i)` represents `rv[i]`.
    ///
    /// `RelPos(predId, i)` represents `i`'th index of the `RelSym` with PredSym-id `predId`.
    ///
    @Internal
    pub enum RamId {
        case Id(Int32)
        case InId(Int32, Int32)
        case TuplePos(RowVar, Int32)
        case RelPos(Int64, Int32)
    }

    instance ToString[RamId] {
        pub def toString(x: RamId): String = match x {
            case (RamId.Id(i1)) => "Id[${i1}]"
            case (RamId.InId(i0, i1)) => "InId[${i0}, ${i1}]"
            case (RamId.TuplePos(i0, i1)) => "TuplePos[${i0}, ${i1}]"
            case (RamId.RelPos(i0, i1)) => "RelPos[${i0}, ${i1}]"
        }
    }
    
    instance Eq[RamId] {
        pub def eq(a: RamId, b: RamId): Bool = match (a, b) {
            case (RamId.Id(i1), RamId.Id(i2)) => i1 == i2
            case (RamId.InId(id1, index1), RamId.InId(id2, index2)) => id1 == id2 and index1 == index2
            case (RamId.TuplePos(i0, i1), RamId.TuplePos(j0, j1)) => i0 == j0 and i1 == j1
            case (RamId.RelPos(i0, i1), RamId.RelPos(j0, j1)) => i0 == j0 and i1 == j1
            case _ => false
        }
    }

    instance Order[RamId] {
        pub def compare(a: RamId, b: RamId): Comparison = match (a, b) {
            case (RamId.Id(i1), RamId.Id(i2)) => i1 <=> i2
            case (RamId.TuplePos(i0, i1), RamId.TuplePos(j0, j1)) =>
                let firstComp = i0 <=> j0;
                if (firstComp != Comparison.EqualTo) firstComp else i1 <=> j1
            case (RamId.RelPos(i0, i1), RamId.RelPos(j0, j1)) =>
                let firstComp = i0 <=> j0;
                if (firstComp != Comparison.EqualTo) firstComp else i1 <=> j1
            case (RamId.InId(i0, i1), RamId.InId(j0, j1)) =>
                let firstComp = i0 <=> j0;
                if (firstComp != Comparison.EqualTo) firstComp else i1 <=> j1
            case (RamId.TuplePos(_, _), _) => Comparison.LessThan
            case (_,  RamId.TuplePos(_, _)) => Comparison.GreaterThan
            case (RamId.RelPos(_, _), _) => Comparison.LessThan
            case (_, RamId.RelPos(_, _)) => Comparison.GreaterThan
            case (RamId.Id(_), _) => Comparison.LessThan
            case (_, RamId.Id(_)) => Comparison.GreaterThan
            case (RamId.InId(_, _), _) => Comparison.LessThan
            case (_, RamId.InId(_, _)) => Comparison.GreaterThan
        }
    }
}
