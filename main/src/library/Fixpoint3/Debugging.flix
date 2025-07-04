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

mod Fixpoint3.Debugging {
    use Fixpoint3.Ast.Ram.RamProgram
    use Fixpoint3.Options

    ///
    /// Invoked after every phase during the fixpoint computation.
    ///
    /// Note: Returns `program` to ensure it is not erased.
    ///
    @Internal
    pub def notifyPreLowering(phase: String, program: RamProgram): Ram.RamProgram = match program {
        case Ram.RamProgram.Program(stmt, _, _, _) => unchecked_cast({
            if (Options.enableDebugging()) region rc {
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
    /// Write `s` to standard file. Overwrite if `overwrite` is true, otherwise append.
    ///
    def flush(s: String, overwrite: Bool): Unit \ IO =
        if (Options.enableDebugToFile()) {
            let res =
                if (overwrite)
                    FileWrite.runWithIO(() -> FileWrite.write(str = s, Options.debugFileName()))
                else
                    FileWrite.runWithIO(() -> FileWrite.append(str = s, Options.debugFileName()));
            match res {
                case Ok(_) => ()
                case Err(e) => println(e)
            }
        } else println(s)

}
