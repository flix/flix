/*
 * Copyright 2022 Nina Andrup Pedersen
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

namespace TestIterable {

    /////////////////////////////////////////////////////////////////////////////
    // MutList                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIterator01(): Bool & Impure =
        MutList.new(): MutList[Int32] |> Iterable.iterator |> Iterator.toList == Nil

    @test
    def toIterator02(): Bool & Impure =
        let u = MutList.new(): MutList[Int32];
        MutList.push!(1, u);
        MutList.push!(2, u);
        MutList.push!(3, u);
        u |> Iterable.iterator |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIterator03(): Bool & Impure =
        MutList.range(-100, 100) |> Iterable.iterator |> Iterator.toList == List.range(-100, 100)

}
