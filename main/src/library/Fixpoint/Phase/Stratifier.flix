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

mod Fixpoint.Phase.Stratifier {
    use Fixpoint.Helpers.unwrap

    use Fixpoint.Ast.Datalog.BodyPredicate.BodyAtom
    use Fixpoint.Ast.Datalog.Constraint.Constraint
    use Fixpoint.Ast.Datalog.Datalog.{Datalog, Model, Join}
    use Fixpoint.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint.Ast.Datalog.{Datalog, Constraint, HeadPredicate, BodyPredicate, Polarity, Fixity}
    use Fixpoint.Ast.Shared.PredSym
    use Fixpoint.PrecedenceGraph
    use Fixpoint.PrecedenceGraph.MutGraph
    use Fixpoint.Ast.Shared.PredSym


    @Internal
    pub def stratify(d: Datalog): Map[PredSym, Int32] = region rc {
        let graph = MutGraph.empty(rc);
        mkDepGraphInternal(graph, d);
        let scc = MutGraph.getStratums(graph);
        let numberOfPredSyms = MutGraph.getSize(graph);
        let predSyms = Array.empty(rc, numberOfPredSyms);
        collectPredSym(predSyms, d);
        let result = MutMap.empty(rc);
        scc |> List.forEachWithIndex(i -> set -> 
            set |>
            Set.forEach(x ->
                result |>
                MutMap.put(
                    PredSym.PredSym(Array.get(x, predSyms), Int32.toInt64(x)), i
                )
            )
        );
        MutMap.toMap(result)
    }

    type alias AdjecencyMap[r: Region] = MutGraph[r]
    type alias PredMap[r: Region] = Array[String, r]


    def mkDepGraphInternal(graph: MutGraph[r], d: Datalog): Unit \ r = match d {
        case Datalog(_, rules) => Vector.forEach(precedenceHelper(graph), rules)
        case Model(_) => ()
        case Join(d1, d2) => mkDepGraphInternal(graph, d1); mkDepGraphInternal(graph, d2)
    }

    def precedenceHelper(graph: MutGraph[r], cnst: Constraint): Unit \ r = match cnst {
        case Constraint(head, body) => Vector.forEach(mkDepEdge(graph, head), body)
    }

    def mkDepEdge(graph: MutGraph[r], dst: HeadPredicate, src: BodyPredicate): Unit \ r = match (dst, src) {
        case (HeadAtom(PredSym.PredSym(_, dstId), _, _), BodyAtom(PredSym.PredSym(_, srcId), _, _, _, _)) => {
            if(dstId != srcId) {
                MutGraph.addEdge(to32(srcId), to32(dstId), graph)
            } else (
                // We still register the edge, as it is needed later
                MutGraph.addVertex(to32(dstId), graph)
            )
        }
        case _ => ()
    }

    def collectPredSym(predMap: PredMap[r], d: Datalog): Unit \ r = match d {
        case Datalog(_, rules) => Vector.forEach(predSymHelper(predMap), rules)
        case Model(_) => ()
        case Join(d1, d2) => collectPredSym(predMap, d1); collectPredSym(predMap, d2)
    }

    def predSymHelper(predMap: PredMap[r], cnst: Constraint): Unit \ r = match cnst {
        case Constraint(HeadPredicate.HeadAtom(PredSym.PredSym(name, id), _, _), body) =>
            Array.put(name, to32(id), predMap);
            Vector.forEach(collectEdge(predMap), body)
    }

    def collectEdge(predMap: PredMap[r], body: BodyPredicate): Unit \ r = match body {
        case (BodyAtom(PredSym.PredSym(name, id), _, _, _, _)) => {
            Array.put(name, to32(id), predMap);
            ()
        }
        case _ => ()
    }

    def to32(i: Int64): Int32 = unwrap(Int64.tryToInt32(i))
}
