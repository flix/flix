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

pub mod Fixpoint3.Ast.Ram {
    use Fixpoint3.Ast.Shared.{BoxedDenotation => Denotation, PredSym}
    use Fixpoint3.Ast.Shared.Denotation.{Relational}
    use Fixpoint3.Boxed
    use Fixpoint3.PredSymsOf
    use Fixpoint3.SubstitutePredSym
    use Fixpoint3.TypeInfo.{provType, RelType, Type, TypeInformation}

    /////////////////////////////////////////////////////////////////////////////
    // RamProgram                                                              //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// A `RamProgram` represents a compiled Datalog program.
    ///
    pub enum RamProgram {
        case Program(RamStmt, FactTuple, Predicates, IndexInformation, TypeInformation)
    }

    ///
    /// Contains a list of `RelSym` representing the names of the relations in the program,
    /// a list of Full `RelSym` and 1 + the maximal identifier of the Full `RelSym`s.
    ///
    pub type alias Predicates = (List[RelSym], List[RelSym], Int64)

    ///
    /// The extensible database (EDB) of a program.
    ///
    pub type alias Facts = Map[RelSym, BPlusTree[Vector[Boxed], Boxed, Static]]

    ///
    /// `FactsTuple` consists of `(oldFacts, newFacts)`, where `oldFacts` are given through
    /// joining a model, while `newFacts` are EDB facts written in the Datalog program.
    ///
    pub type alias FactTuple = (Facts, Facts)

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
    pub type alias Search = Vector[Int32]

    ///
    /// Maps to the set of `Search` for a `RelSym` in the Datalog program. It represents
    /// the indexes that will be created for `RelSym` during program evaluation.
    ///
    pub type alias Indexes = Map[RelSym, Vector[Search]]

    ///
    /// Maps a `RelSym` and its place in `Indexes[RelSym]` to its final position in memory.
    ///
    pub type alias IndexPositions = Map[(RelSym, Int32), Int32]

    ///
    /// The indexes to be created during program evaluation and their positions in memory.
    ///
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
    pub enum RamStmt {
        case Insert(RelOp)
        case MergeInto(RelSym, RelSym)
        case Swap(RelSym, RelSym)
        case Purge(RelSym)
        case Seq(Vector[RamStmt])
        case Par(Vector[RamStmt])
        case Until(Vector[BoolExp], RamStmt)
        case Comment(String)
    }

    instance ToString[RamStmt] {
        pub def toString(stmt: RamStmt): String =
            let nl = String.lineSeparator();
            match stmt {
                case RamStmt.Insert(op)          => ToString.toString(op)
                case RamStmt.MergeInto(src, dst) => "Merge ${src} into ${dst}"
                case RamStmt.Swap(lhs, rhs)      => "Swap ${lhs} and ${rhs}"
                case RamStmt.Purge(relSym)       => "Purge ${relSym}"
                case RamStmt.Seq(xs)             => Vector.join(";${nl}", xs)
                case RamStmt.Par(xs)             => Vector.join("|${nl}", xs)
                case RamStmt.Until(test, body)   =>
                    let tst = test |> Vector.join(" && ");
                    "until(${tst}) do${nl}${String.indent(4, "${body}")}end"
                case RamStmt.Comment(comment)    => "/* ${comment} */"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RelOp                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// `Search(rv, relSym, relType, body)` iterates through the facts in relation
    /// `relSym`, saving them as RowVar `rv` and executes `body`.
    ///
    /// `Query(rv, relSym, bools, indexPos, relType, body)` iterates through the facts in relation
    /// `relSym`, saving them as `rv` and executes `body` if `bools` is
    /// true. The index saved at `indexPos` will be used.
    ///
    /// `Functional(rv, f, input, body, arity, relType)` evaluates `f(input)`, for each result,
    /// saves it as `rv` and executes `body`. `arity` is the arity of the output tuples.
    ///
    /// `Project(terms, relSym, ruleNum)` constructs a tuple from `terms` and inserts it as a fact
    /// for relation `relSym`. `relSym` should be a new relation. `ruleNum` is a unique number assigned
    /// to the rule the project statement is associated with.
    ///
    /// `If(bools, body)` evaluates `body` if `bools` is true.
    ///
    pub enum RelOp {
        case Search(RowVar, RelSym, RelType, RelOp)
        case Query(RowVar, RelSym, Vector[BoolExp], Int32, RelType, RelOp)
        case Functional(RowVar, Vector[Boxed] -> Vector[Vector[Boxed]], Vector[RamTerm], RelOp, Int32, RelType)
        case Project(Vector[RamTerm], RelSym, Int32)
        case If(Vector[BoolExp], RelOp)
    }

    instance ToString[RelOp] {
        pub def toString(op: RelOp): String =
            let nl = String.lineSeparator();
            match op {
                case RelOp.Search(var, relSym, _, body) =>
                    "search ${var} ∈ ${relSym} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Query(var, relSym, prefixQuery, _, _, body) =>
                    let qry = Vector.joinWith(match term -> {
                        ToString.toString(term)
                    }, " ∧ ", prefixQuery);
                    "query {${var} ∈ ${relSym} | ${qry}} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Functional(RowVar.Named(id), _, terms, body, _, _) =>
                    "loop(x${id} <- f(${terms |> Vector.join(", ")})) do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Project(terms, relSym, _) =>
                    "project (${terms |> Vector.join(", ")}) into ${relSym}"
                case RelOp.If(test, then) =>
                    let tst = test |> Vector.join(" ∧ ");
                    "if (${tst}) then${nl}${String.indent(4, "${then}")}end"
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
    /// `NotMemberOf(terms, relSym, repNegative)`: True if the tuple constructed from `terms` is not
    /// in relation `relSym`. `repNegative` is true if `NotMemberOf` is introduced by a `not A(...)`.
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
    pub enum BoolExp {
        case Not(BoolExp)
        case IsEmpty(RelSym)
        case NotMemberOf(Vector[RamTerm], RelSym, Bool)
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
                case BoolExp.Not(boolExp)                  => "not (${boolExp})"
                case BoolExp.IsEmpty(relSym)               => "${relSym} = ∅"
                case BoolExp.NotMemberOf(terms, relSym, _) => "(${terms |> Vector.join(", ")}) ∉ ${relSym}"
                case BoolExp.NotBot(term, _, _)            => "(${term}) ≠ ⊥"
                case BoolExp.Leq(elem, _, term)            => "${elem} ≤ (${term})"
                case BoolExp.Eq(lhs, rhs)                  => "${lhs} = ${rhs}"
                case BoolExp.Guard1(_, v)                  => "<clo>(${v})"
                case BoolExp.Guard2(_, v1, v2)             => "<clo>(${v1}, ${v2})"
                case BoolExp.Guard3(_, v1, v2, v3)         => "<clo>(${v1}, ${v2}, ${v3})"
                case BoolExp.Guard4(_, v1, v2, v3, v4)     => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case BoolExp.Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RamTerm                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Represents a Relational Algebra Machine (RAM) term.
    ///
    /// `Lit(val, id)` represents the literal `val`.
    ///
    /// `ProvMax([(rv1, i1), (rv2, i2), (rv3, i3), ...])` where represents computing
    /// `max(rv1[i1], rv2[i2], rv3[i3], ...)`.
    ///
    /// `RowLoad(x, i, type, relSym)` represents an index into the tuple
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
    pub enum RamTerm {
        case Lit(Boxed, Type)
        case ProvMax(Vector[(RowVar, Int32)])
        case RowLoad(RowVar, Int32, Type, RelSym)
        case Meet(Boxed -> Boxed -> Boxed, RamTerm, (RowVar, RelSym), Type)
        case App1(Boxed -> Boxed, RamTerm, Type)
        case App2(Boxed -> Boxed -> Boxed, RamTerm, RamTerm, Type)
        case App3(Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, Type)
        case App4(Boxed -> Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, Type)
        case App5(Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, RamTerm, Type)
    }

    instance ToString[RamTerm] {
        pub def toString(term: RamTerm): String = match term {
            case RamTerm.Lit(v, _)                      => Debug.stringify(v)
            case RamTerm.ProvMax(loads)                 =>
                let loadsStr = Vector.joinWith(match (rv, i) -> "${rv}[${i}]", ", ", loads);
                "max(${loadsStr})"
            case RamTerm.RowLoad(var, index, _, _)      => "${var}[${index}]"
            case RamTerm.Meet(_, lhs, rhs, _)           => "(${lhs} ⊓ ${rhs})"
            case RamTerm.App1(_, v, _)                  => "<clo>(${v})"
            case RamTerm.App2(_, v1, v2, _)             => "<clo>(${v1}, ${v2})"
            case RamTerm.App3(_, v1, v2, v3, _)         => "<clo>(${v1}, ${v2}, ${v3})"
            case RamTerm.App4(_, v1, v2, v3, v4, _)     => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
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
    /// Returns a `RelSym` wrapper of the `predSym`.
    ///
    /// The returned `RelSym` is equivalent to a 'real' `RelSym` with respect
    /// to the `Order` on `RelSym`.
    ///
    /// Used for accessing values in maps.
    ///
    pub def makeFakeRelSym(predSym: PredSym): RelSym = RelSym.Symbol(predSym, -1, Relational)

    ///
    /// Returns the arity of `relSym`.
    ///
    pub def arityOf(relSym: RelSym): Int32 = match relSym {
        case RelSym.Symbol(_, arity, _) => arity
    }

    ///
    /// Returns the arity of the non-lattice part of `relSym`.
    ///
    pub def arityOfNonLat(relSym: RelSym): Int32 = match relSym {
        case RelSym.Symbol(_, arity, den) => match den {
            case Relational => arity
            case _          => arity - 1
        }
    }

    ///
    /// Returns the denotation of `relSym`.
    ///
    pub def toDenotation(relSym: RelSym): Denotation = match relSym {
        case RelSym.Symbol(_, _, den) => den
    }

    ///
    /// Returns the identifier of `relSym`.
    ///
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

}
