/*
 *  Copyright 2020 Matthew Lutze
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

namespace TestToString {
    use Functor.map;

    @test
    def testMapOption(): Bool =
        map(x -> x + 1, Some(41)) == Some(42)

    @test
    def testMapList(): Bool =
        map(x -> x + 1, 3 :: 4 :: 5 :: Nil) == 4 :: 5 :: 6 :: Nil
}
