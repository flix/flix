/*
 * Copyright 2022 Jonathan Lindegaard Starup
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

/// This code is based on this souffle example
/// https://github.com/souffle-lang/souffle/tree/master/tests/example/array
pub def arrayNeighbourhood(default: Int32, values: List[(Int32, Int32)], parameters: Int32):
#{ Neighbourhood(Int32, Int32, Int32, Int32) | r } =

    let defaultP = #{ Default(default). };
    let valuesP = inject values into Values;
    let parametersP = #{ Parameters(parameters). };
    let input = defaultP <+> valuesP <+> parametersP;
    let p = #{
        // Added for Flix
        ValuesIndex(i) :- Values(i, _).
        ElementIndex(i) :- Element(i, _).
        ElementRight(i-1, x) :- Element(i, x).
        ElementLeft(i+1, x) :- Element(i, x).

        // Original Program

        Indices(0).
        Indices(i+1) :- Indices(i), Parameters(l), if i >= 0 and i+1 < l.

        // The elements of the array, referenced by an index, and having a specified value.
        // Note that if a value is not specified an element takes the default value.
        Element(i, d) :- Indices(i), not ValuesIndex(i), Default(d).
        Element(i, x) :- Indices(i), Values(i, x).

        // The value of the element to the left of the element specified by
        // the index, or the default value if it is out of the array bounds.
        Left(i+1, d) :- not ElementIndex(i), ElementRight(i, _), Default(d).
        Left(i, x) :- ElementLeft(i, x), Element(i, _).

        // The value of the element to the right of the element specified by the
        // index, or the default value if it is out of bounds.
        Right(i-1, d) :- not ElementIndex(i), ElementLeft(i, _), Default(d).
        Right(i, x) :- ElementRight(i, x), Element(i, _).

        // The local neighbourhood of a specified element, giving the value of that
        // element, and the elements surrounding it.
        Neighbourhood(i, l, c, r) :- Left(i, l), Element(i, c), Right(i, r).
    };
    solve input, p inject Neighbourhood

def main(): Unit & Impure =
    let values = (1, 13) :: (2, 21) :: (3, 34) :: (5, 55) :: (8, 89) :: Nil;
    let length = 10;
    let default = 0;

    println("x is an array of length ${length} where:");
    values |> List.map(v -> "  x[${fst(v)}] = ${snd(v)}") |> List.foreach(println);
    println("and every other element is ${default}.");
    println("");

    let neighbourhood =
        query arrayNeighbourhood(default, values, length)
        select (i, l, c, r)
        from Neighbourhood(i, l, c, r);

    println("The for every index, the value and the neighbouring values are given.");
    println("The neighbourhood, N, is defined out of bounds to be the default value ${default}");
    println("This is shown as '..., N(i-1), N(i), N(i+1), ...");
    neighbourhood |>
        Array.map(match (i, l, c, r) -> "${i}: ..., ${l}, ${c}, ${r}, ...") |>
        Array.foreach(println)
