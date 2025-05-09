/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

mod Benchmark.Enum {

    enum Formula {
        case Lit(Int32)
        case Var(String)
        case Plus(Nel[Formula])
    }

    pub def variables(f: Formula): Set[String] = match f {
        case Formula.Lit(_) => Set.empty()
        case Formula.Var(s) => Set.singleton(s)
        case Formula.Plus(xs) => xs |> Nel.map(variables) |> Nel.reduce
    }

    pub def genExample(n: Int32): Formula = region local {
        let count = Ref.fresh(local, 0);
        genExampleAux(count, n)
    }

    def genExampleAux(gen: Ref[Int32, r], n: Int32): Formula \ r = {
        if (n <= 3) {
            Formula.Plus(Nel.Nel(
                Formula.Lit(Ref.get(gen)),
                List#{
                    Formula.Var(ToString.toString(Ref.get(gen))),
                    Formula.Lit(Ref.get(gen))
                }
            ))
        } else {
            Formula.Plus(Nel.Nel(
                genExampleAux(gen, n/3),
                List#{
                    genExampleAux(gen, n/3),
                    genExampleAux(gen, n/3)
                }
            ))
        }
    }

}
