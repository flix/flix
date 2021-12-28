/*
 *  Copyright 2021 Stephen Tetley
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

namespace TestMonad {

    use Monad.{flatten, kleisliLeft, kleisliRight};

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        let x: Option[Option[Int32]] = None;
        flatten(x) == None

    @test
    def flatten02(): Bool =
        let x: Option[Option[Int32]] = Some(None);
        flatten(x) == None

    @test
    def flatten03(): Bool =
        let x: Option[Option[Int32]] = Some(Some(1));
        flatten(x) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // kleisliLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def kleisliLeft01(): Bool =
        kleisliLeft(_ -> None, x2 -> Some(ToString.toString(x2)), 1) == None

    @test
    def kleisliLeft02(): Bool =
        kleisliLeft(x1 -> Some(x1 + 1), x2 -> Some(ToString.toString(x2)), 1) == Some("2")

    /////////////////////////////////////////////////////////////////////////////
    // kleisliRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def kleisliRight01(): Bool =
        kleisliRight(x1 -> Some(ToString.toString(x1)), _ -> None, 1) == None

    @test
    def kleisliRight02(): Bool =
        kleisliRight(x1 -> Some(ToString.toString(x1)), x2 -> Some(x2 + 1), 1) == Some("2")

}
