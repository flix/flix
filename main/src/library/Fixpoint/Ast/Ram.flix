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

mod Fixpoint.Ast.Ram {
    use Fixpoint.Ast.Shared.Denotation
    use Fixpoint.Ast.Shared.PredSym
    use Fixpoint.Boxed;
    use Fixpoint.Predicates.PredSymMapInternal
    use Fixpoint.PredSymsOf
    use Fixpoint.SubstitutePredSym

    /////////////////////////////////////////////////////////////////////////////
    // RamProgram                                                              //
    /////////////////////////////////////////////////////////////////////////////

    // A program consists of statements, the EDB, predicate renaming information,
    // and a set of indices to be constructed.
    pub enum RamProgram {
        case Program(RamStmt, Facts, PredicateState, IndexInformation)
    }

    pub type alias PredicateState = Fixpoint.Predicates.PredSymMapInternal
    pub type alias Facts = Map[RelSym, Vector[Vector[Boxed]]]
    pub type alias Search = Vector[Int32]
    pub type alias ConstructedIndexes = Map[RelSym, Vector[Search]]
    // The first is a list of the indices that should be made. The second is a map 
    // describing where they are suppoesed to be saved at runtime.
    pub type alias IndexInformation = (ConstructedIndexes, Map[(RelSym, Int32), Int32])

    /////////////////////////////////////////////////////////////////////////////
    // RamStmt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Internal
    pub enum RamStmt {
        case Insert(RelOp)
        case Merge(RelSym, RelSym)
        case Swap(RelSym, RelSym)
        case Purge(RelSym)
        case Seq(Vector[RamStmt])
        case Par(Vector[RamStmt])
        case Until(Vector[BoolExp], RamStmt)
        case Comment(String)
        // (RelSym, index, position to save result, search)
        case EstimateJoinSize(RelSym, Int32, Int32, Vector[Int32])
    }

    instance ToString[RamStmt] {
        pub def toString(stmt: RamStmt): String =
            let nl = String.lineSeparator();
            match stmt {
                case RamStmt.Insert(op) => ToString.toString(op)
                case RamStmt.Merge(src, dst) => "merge ${src} into ${dst}"
                case RamStmt.Swap(lhs, rhs) => "Swap ${lhs} and ${rhs}"
                case RamStmt.Purge(relSym) => "purge ${relSym}"
                case RamStmt.Seq(xs) => Vector.join(";${nl}", xs)
                case RamStmt.Until(test, body) =>
                    let tst = test |> Vector.join(" && ");
                    "until(${tst}) do${nl}${String.indent(4, "${body}")}end"
                case RamStmt.Comment(comment) => "// ${comment}"
                case RamStmt.Par(xs) => Vector.join("|${nl}", xs)
                case RamStmt.EstimateJoinSize(relSym, index, writeTo, attr) => 
                    "EstimateJoinSize(${relSym}, {${Vector.join(", ", attr)}}) @ ${index}, ${writeTo}"
            }
    }

    /////////////////////////////////////////////////////////////////////////////
    // RelOp                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    
    @Internal
    pub enum RelOp {
        // Loop through RelSym, saving values in RowVar with body of loop RelOp.
        case Search(RowVar, RelSym, RelOp) 
        case Query(RowVar, RelSym, Vector[BoolExp], Int32, RelOp)
        // The last is the arity
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

    @Internal
    pub enum BoolExp {
        case Not(BoolExp)
        case IsEmpty(RelSym)
        case NotMemberOf(Vector[RamTerm], RelSym)
        // (term, leq, bot). term must be a Meet or RowLoad of a lattice var.
        case NotBot(RamTerm, Boxed -> Boxed -> Bool, Boxed)
        // (lowerBound, rowVar, relSym).
        case Leq(Boxed, RowVar, RelSym)
        case Eq(RamTerm, RamTerm)
        case Guard0(Unit -> Bool)
        case Guard1(Boxed ->Bool, RamTerm)
        case Guard2(Boxed -> Boxed  -> Bool, RamTerm, RamTerm)
        case Guard3(Boxed -> Boxed  -> Boxed -> Bool, RamTerm, RamTerm, RamTerm)
        case Guard4(Boxed -> Boxed  -> Boxed -> Boxed -> Bool, RamTerm, RamTerm, RamTerm, RamTerm)
        case Guard5(Boxed -> Boxed  -> Boxed -> Boxed -> Boxed -> Bool, RamTerm, RamTerm, RamTerm, RamTerm, RamTerm)
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
    /// `RowLoad(x, i)` represents an index into the tuple
    /// bound to the local variable `x`, i.e. `x[i]`.
    ///
    /// `LoadLatVar(x)` represents the lattice component in the tuple
    /// bound to the local variable `x`.
    ///
    /// The RamId uniquely identifies the term
    @Internal
    pub enum RamTerm {
        case Lit(Boxed, RamId)
        case RowLoad(RowVar, Int32, RelSym)
        case Meet(Boxed -> Boxed  -> Boxed, RamTerm, (RowVar, RelSym), RamId)
        case App0(Unit -> Boxed, RamId)
        case App1(Boxed -> Boxed, RamTerm, RamId)
        case App2(Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamId)
        case App3(Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamId)
        case App4(Boxed -> Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, RamId)
        case App5(Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Boxed, RamTerm, RamTerm, RamTerm, RamTerm, RamTerm, RamId)
    }

    pub def getTermRamId(t: RamTerm): RamId = match t {
        case RamTerm.Lit(_, id) => id
        case RamTerm.RowLoad(RowVar.Named(i), index, _) => RamId.TuplePos(i, index)
        case RamTerm.Meet(_, _, _, id) => id
        case RamTerm.App0(_, id) => id
        case RamTerm.App1(_ , _, id) => id
        case RamTerm.App2(_, _, _, id) => id
        case RamTerm.App3(_, _, _, _, id) => id
        case RamTerm.App4(_, _, _, _, _, id) => id
        case RamTerm.App5(_, _, _, _, _, _, id) => id
    }

    pub def getLatVarRamId(rv: RowVar, relSym: RelSym): RamId =
        let arity = arityOfNonLat(relSym);
        let RowVar.Named(id) = rv;
        RamId.TuplePos(id, arity)


    instance ToString[RamTerm] {
        pub def toString(term: RamTerm): String = match term {
            case RamTerm.Lit(v, _) => "%{v}"
            case RamTerm.RowLoad(var, index, _) => "${var}[${index}]"
            case RamTerm.Meet(_, lhs, rhs, _) => "(${lhs} ⊓ ${rhs})"
            case RamTerm.App0(_, _) => "<clo>()"
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

    /**
        Represents the relations (also delta relations) of the program.
            The first is the symbol of the relation
            The second parameter is the arity
            The third depends denotes whether we are dealing with a lattice or a 'normal' relation.
    */

    @Internal
    pub enum RelSym {
        case Symbol(PredSym, Int32, Denotation)
    }

    instance Eq[RelSym] {
        pub def eq(a: RelSym, b: RelSym): Bool = match (a, b) {
            case (RelSym.Symbol(s1, _, _),  RelSym.Symbol(s2, _, _))  => s1 == s2
        }
    }

    instance Order[RelSym] {
        pub def compare(a: RelSym, b: RelSym): Comparison = match (a, b) {
            case (RelSym.Symbol(s1, _, _), RelSym.Symbol(s2, _, _)) =>  s1 <=> s2
        }
    }

    instance PredSymsOf[RelSym] {
        pub def predSymsOf(x: RelSym): Set[PredSym] = match x {
            case RelSym.Symbol(predSym, _, _)    => Set.singleton(predSym)
        }
    }

    instance SubstitutePredSym[RelSym] {
        pub def substitute(x: RelSym, s: Map[PredSym, PredSym]): RelSym = match x {
            case RelSym.Symbol(predSym, arity, den)  =>
                let newSym = Map.getWithDefault(predSym, predSym, s);
                RelSym.Symbol(newSym, arity, den)
        }
    }

    instance ToString[RelSym] {
        pub def toString(relSym: RelSym): String = match relSym {
            case RelSym.Symbol(sym, _, _)    => "${sym}"
        }
    }

    @Internal
    pub def toPredID(relSym: RelSym): Int64 = match relSym {
        case RelSym.Symbol(PredSym.PredSym(_, id), _, _) => id
    }

    @Internal
    pub def arityOf(relSym: RelSym): Int32 = match relSym {
        case RelSym.Symbol(_, arity, _) => arity
    }

    @Internal
    pub def arityOfNonLat(relSym: RelSym): Int32 = match relSym {
        case RelSym.Symbol(_, arity, den) => match den {
            case Denotation.Relational => arity
            case _ => arity - 1
        }
    }

    @Internal
    pub def toDenotation(relSym: RelSym): Denotation = match relSym {
        case RelSym.Symbol(_, _, den) => den
    }

    @Internal
    pub def toId(relSym: RelSym): Int64 = match relSym {
        case RelSym.Symbol(PredSym.PredSym(_, id), _, _) => id
    }

    @Internal
    pub def collectRelSym(program: RamProgram): List[RelSym] = region _rc { 
        match program {
            case RamProgram.Program(_, _, predState, _) =>
                Fixpoint.Predicates.allRelSyms(predState)
        }
    }

    @Internal
    pub def collectRowVarOp(op: RelOp): List[RowVar] = match op {
        case RelOp.Search(rv, _, rest) => rv :: collectRowVarOp(rest)
        case RelOp.Query(rv, _, _, _, rest) => rv :: collectRowVarOp(rest)
        case RelOp.Functional(_, _, _, rest, _) => collectRowVarOp(rest)
        case RelOp.Project(_, _) => Nil
        case RelOp.If(_, rest) => collectRowVarOp(rest)
    }

    @Internal
    pub def collectRowVarRelSym(program: RamProgram): List[(RowVar, RelSym)] = match program {
        case RamProgram.Program(stmt, _, _, _) => 
            collectRowVarRelSymStmt(stmt)
    }

    @Internal
    pub def collectRowVarArity(program: RamProgram): List[(RowVar, Int32)] = 
        collectRowVarRelSym(program) |> List.map(match (rv, RelSym.Symbol(_, arity, _)) -> (rv, arity))
    

    @Internal
    def collectRowVarRelSymStmt(stmt: RamStmt): List[(RowVar, RelSym)] \ r = match stmt {
        case RamStmt.Insert(op) => collectRowVarRelSymOp(op)
        case RamStmt.Merge(_, _) => Nil
        case RamStmt.Swap(_, _) => Nil
        case RamStmt.Purge(_) => Nil
        case RamStmt.Seq(stmts) => Vector.foldLeft(acc -> x -> List.append(collectRowVarRelSymStmt(x), acc), Nil, stmts)
        case RamStmt.Par(stmts) => Vector.foldLeft(acc -> x -> List.append(collectRowVarRelSymStmt(x), acc), Nil, stmts)
        case RamStmt.Until(_, rest) => collectRowVarRelSymStmt(rest)
        case RamStmt.Comment(_) => Nil
        case RamStmt.EstimateJoinSize(_, _, _, _) => Nil
    }

    @Internal
    pub def collectRowVarRelSymOp(op: RelOp): List[(RowVar, RelSym)] \ r = match op {
        case RelOp.Search(rv, relSym, rest) => (rv, relSym) :: collectRowVarRelSymOp(rest)
        case RelOp.Query(rv, relSym, _, _, rest) => (rv, relSym) :: collectRowVarRelSymOp(rest)
        case RelOp.Functional(_, _, _, rest, _) => collectRowVarRelSymOp(rest)
        case RelOp.Project(_, _) => Nil
        case RelOp.If(_, rest) => collectRowVarRelSymOp(rest)
    }

    /////////////////////////////////////////////////////////////////////////////
    // RowVar                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    /**
    Note that the id is supposed to be unique

    For code:
        search e1 ∈ Edge  do 
            search e2 ∈ Edge do
                ...

    'e1' and 'e2' would be the RowVars.
    */

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

    // A RamId is is either an identifier on a RAM-instruction, or an index in a RelSym.
    // Identifiers should be <0 for RamId.Id. Might be avoidable
    pub enum RamId {
        case Id(Int32) // (ID)
        case InId(Int32, Int32) // (OutID, index)
        case TuplePos(Int32, Int32) // (RowVar, index)
        case RelPos(Int64, Int32) // (PredSym, index). This does not exist in the RAM, but is useful for Boxing.
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
