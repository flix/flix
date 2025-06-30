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

mod Fixpoint3.Phase.Stratifier {
    use Fixpoint3.Ast.Datalog.BodyPredicate.BodyAtom
    use Fixpoint3.Ast.Datalog.Constraint.Constraint
    use Fixpoint3.Ast.Datalog.Datalog.{Datalog, Model, Join}
    use Fixpoint3.Ast.Datalog.{Datalog, Constraint, HeadPredicate, BodyPredicate, Polarity, Fixity}
    use Fixpoint3.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint3.Ast.Shared.PredSym
    use Fixpoint3.Util.getOrCrash
    use Fixpoint3.PrecedenceGraph.{MutGraph, getPseudoStrata}

    ///
    /// Array allowing lookup of the id-part of `PredSym` to name-part of `PredSym`.
    ///
    type alias PredIdToName[r: Region] = Array[String, r]

    ///
    /// Computes the stratification of `d`. The result is a map from `PredSym` to pseudo
    /// strata and a their position in the pseudo-stratum. The number is meaningless,
    /// except for allowing to seperate `PredSym` in the same pseudo-stratum by which
    /// strata they belong to to.
    ///
    @Internal
    pub def stratify(d: Datalog): Map[PredSym, (Int32, Int32)] = region rc {
        let graph = MutGraph.empty(rc);
        mkDepGraphInternal(graph, d);
        let scc = getPseudoStrata(graph);
        let numberOfPredSyms = MutGraph.getSize(graph);
        let predSyms = Array.empty(rc, numberOfPredSyms);
        collectPredSym(predSyms, d);
        let result = MutMap.empty(rc);
        scc |> List.forEachWithIndex(pseudoStrat -> list ->
            list |> List.forEachWithIndex(strat -> set ->
                set |> Set.forEach(x -> {
                    let predSym = PredSym.PredSym(Array.get(x, predSyms), Int32.toInt64(x));
                    MutMap.put(predSym, (pseudoStrat, strat), result)
                })
            )
        );
        MutMap.toMap(result)
    }

    ///
    /// Insert edges into `graph` for the dependencies in the Datalog program `d`.
    /// `A(x) :- B(x), A(x), not C(x).` inserts the edge `B -> A` and `C -> A` in `graph`.
    ///
    def mkDepGraphInternal(graph: MutGraph[r], d: Datalog): Unit \ r = match d {
        case Datalog(_, rules) => Vector.forEach(precedenceHelper(graph), rules)
        case Model(_) => ()
        case Join(d1, d2) => mkDepGraphInternal(graph, d1); mkDepGraphInternal(graph, d2)
    }

    ///
    /// Inserts edges into `graph` for the dependencies in the constraints `cnst`.
    ///
    /// See `mkDepGraphInternal`.
    ///
    def precedenceHelper(graph: MutGraph[r], cnst: Constraint): Unit \ r = match cnst {
        case Constraint(head, body) => Vector.forEach(mkDepEdge(graph, head), body)
    }

    ///
    /// Inserts edges into `graph` for the found dependency between `dst` and `src`. No
    /// edge is inserted if `dst == src`.
    ///
    /// See `mkDepGraphInternal`.
    ///
    def mkDepEdge(graph: MutGraph[r], dst: HeadPredicate, src: BodyPredicate): Unit \ r = match (dst, src) {
        case (HeadAtom(PredSym.PredSym(_, dstId), _, _), BodyAtom(PredSym.PredSym(_, srcId), _, _, _, _)) => {
            if(dstId != srcId) {
                MutGraph.addEdge(toInt32(srcId), toInt32(dstId), graph)
            } else (
                // We still register the vertex, as it is needed later.
                MutGraph.addVertex(toInt32(dstId), graph)
            )
        }
        case _ => ()
    }

    ///
    /// Collects the names of the `PredSym`s of `d`. A `PredSym` `(name, id)` in `d` will
    /// ensure that `predMap[id] == name`.
    ///
    def collectPredSym(predMap: PredIdToName[r], d: Datalog): Unit \ r = match d {
        case Datalog(_, rules) => Vector.forEach(predSymHelper(predMap), rules)
        case Model(_) => ()
        case Join(d1, d2) => collectPredSym(predMap, d1); collectPredSym(predMap, d2)
    }

    ///
    /// Collects the names of the `PredSym`s of `cnst`.
    ///
    /// See `collectPredSym`.
    ///
    def predSymHelper(predMap: PredIdToName[r], cnst: Constraint): Unit \ r = match cnst {
        case Constraint(HeadPredicate.HeadAtom(PredSym.PredSym(name, id), _, _), body) =>
            Array.put(name, toInt32(id), predMap);
            Vector.forEach(collectEdge(predMap), body)
    }

    ///
    /// Collects the names of the `PredSym`s of `body`.
    ///
    /// See `collectPredSym`.
    ///
    def collectEdge(predMap: PredIdToName[r], body: BodyPredicate): Unit \ r = match body {
        case (BodyAtom(PredSym.PredSym(name, id), _, _, _, _)) => {
            Array.put(name, toInt32(id), predMap);
            ()
        }
        case _ => ()
    }

    ///
    /// Cast `i` to an `Int32`. Crashes if there are more than 2^32 `PredSyms`s.
    ///
    def toInt32(i: Int64): Int32 = getOrCrash(Int64.tryToInt32(i))
}
