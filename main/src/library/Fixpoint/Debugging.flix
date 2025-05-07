/*
 * Copyright 2021 Magnus Madsen
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

mod Fixpoint.Debugging {

    use Fixpoint.Ast.Datalog.Datalog
    use Fixpoint.Ast.ExecutableRam
    use Fixpoint.Ast.Ram
    use Fixpoint.Ast.Shared.PredSym
    use Fixpoint.Boxing

    def flush(s: String, overwrite: Bool): Unit \ IO = 
        if (Fixpoint.Options.enableDebugToFile()) {
            let res = 
                if (overwrite) 
                    FileWrite.runWithIO(() -> FileWrite.write(str = s, Fixpoint.Options.debugFileName()))
                else 
                    FileWrite.runWithIO(() -> FileWrite.append(str = s, Fixpoint.Options.debugFileName()));
            match res {
                case Ok(_) => ()
                case Err(e) => println(e)
            } 
        } else println(s)


    ///
    /// Invoked before the fixpoint computation.
    ///
    /// Note: Returns `d` to ensure it is not erased.
    ///
    @Internal
    pub def notifyPreSolve(d: Datalog): Datalog = unchecked_cast({
        if (Fixpoint.Options.enableDebugging()) region rc {
            let strings = MutList.empty(rc);
            let psh = s -> MutList.push(s, strings);
            String.repeat(80, "*") |> psh;
            "** Datalog Input" |> psh;
            String.repeat(80, "*") |> psh;
            "" |> psh;
            String.indent(4, "${d}") |> psh;
            "" |> psh;
            flush(MutList.join("\n", strings), true);
            d
        } else {
            d
        }
    } as _ \ {})

    ///
    /// Invoked with the minimal model of the fixpoint computation.
    ///
    /// Note: Returns `d` to ensure it is not erased.
    ///
    @Internal
    pub def notifyPostSolve(d: Datalog): Datalog = unchecked_cast({
        if (Fixpoint.Options.enableDebugging() and Fixpoint.Options.enableDebugPrintFacts()) region rc {
            let strings = MutList.empty(rc);
            let psh = s -> MutList.push(s, strings);
            String.repeat(80, "*") |> psh;
            "** Minimal Model" |> psh;
            String.repeat(80, "*") |> psh;
            "" |> psh;
            String.indent(4, "${d}") |> psh;
            "" |> psh;
            flush(MutList.join("\n", strings), false);
            d
        } else {
            d
        }
    } as _ \ {})

    ///
    /// Invoked after every phase during the fixpoint computation.
    ///
    /// Note: Returns `d` to ensure it is not erased.
    ///
    @Internal
    pub def notifyPreLowering(phase: String, program: Ram.RamProgram): Ram.RamProgram = match program {
        case Ram.RamProgram.Program(stmt, _, _, _) => unchecked_cast({
            if (Fixpoint.Options.enableDebugging()) region rc {
                let strings = MutList.empty(rc);
                let psh = s -> MutList.push(s, strings);
                String.repeat(80, "*") |> psh;
                "** (${phase}) Relation Algebra Machine AST" |> psh;
                String.repeat(80, "*") |> psh;
                "" |> psh;                
                String.indent(4, "${stmt}") |> psh;
                ""  |> psh;
                flush(MutList.join("\n", strings), false);
                program
            } else {
                program
            }
        } as _ \ {})
    }

    ///
    /// Invoked before the fixpoint computation.
    ///
    /// Note: Returns `s` to ensure it is not erased.
    ///
    @Internal
    pub def notifyPreInterpret(
        input: (ExecutableRam.RamProgram, Boxing.Boxing[rc])
    ): (ExecutableRam.RamProgram, Boxing.Boxing[rc]) = match fst(input) {
        case ExecutableRam.RamProgram.Program(stmt, facts, _meta, (index, indexPos), _, _) => unchecked_cast({
            if (Fixpoint.Options.enableDebugging()) region rc {
                let strings = MutList.empty(rc);
                let psh = s -> MutList.push(s, strings);
                if (Fixpoint.Options.enableDebugPrintFacts()) {
                    String.repeat(80, "*") |> psh;
                    "** Facts" |> psh;
                    String.repeat(80, "*") |> psh;
                    "" |> psh;
                    String.indent(4, "${unchecked_cast(facts as Map[Fixpoint.Ast.Ram.RelSym, Vector[(Vector[Int64], Fixpoint.Boxed)]])}") |> psh;
                    String.indent(4, "${Debug.stringify(facts)}") |> psh;
                    "" |> psh
                } else ();
                String.repeat(80, "*") |> psh;
                "** Executable Relation Algebra Machine AST" |> psh;
                String.repeat(80, "*") |> psh;
                "" |> psh;
                String.indent(4, "${stmt}") |> psh;
                "" |> psh;
                String.repeat(80, "*") |> psh;
                "** Index Information" |> psh;
                String.repeat(80, "*") |> psh;
                "" |> psh;
                String.indent(4, Map.joinWith(k -> v -> "${k}: ${v}", "\n", index)) |> psh;
                "" |> psh;
                String.repeat(80, "*") |> psh;
                "** Index Position in Database" |> psh;
                String.repeat(80, "*") |> psh;
                "" |> psh;
                String.indent(4, Map.joinWith(k -> v -> "${k}: ${v}", "\n", indexPos)) |> psh;
                "" |> psh;
                flush(MutList.join("\n", strings), false);
                input
            } else {
                input
            }
        } as _ \ {})
    }

}
