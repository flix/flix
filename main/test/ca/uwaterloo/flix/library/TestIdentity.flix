/*
 *  Copyright 2022 Stephen Tetley
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

mod TestIdentity {
    use Identity.Identity


    /////////////////////////////////////////////////////////////////////////////
    // SemiGroup                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine01(): Bool =
        SemiGroup.combine(Identity(1 :: Nil), Identity(2 :: Nil)) == Identity(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // Monoid                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Bool =
        Identity(Monoid.empty() : List[Int32]) == Identity(Nil): Identity[List[Int32]]

    /////////////////////////////////////////////////////////////////////////////
    // Functor                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Bool =
        Functor.map(x -> x + 1, Identity(0)) == Identity(1)

    /////////////////////////////////////////////////////////////////////////////
    // Applicative                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def point01(): Bool =
        Applicative.point(1) : Identity[Int32] == Identity(1)

    @Test
    def ap01(): Bool =
        Applicative.ap(Identity(x -> x+ 1), Identity(1)) == Identity(2)

    /////////////////////////////////////////////////////////////////////////////
    // Monad                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Bool =
        Monad.flatMap(x -> Identity(x+1), Identity(1)) == Identity(2)

    /////////////////////////////////////////////////////////////////////////////
    // Foldable                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool =
        Foldable.foldLeft((s, x) -> s+x, 100, Identity(1)) == 101

    @Test
    def foldRight01(): Bool =
        Foldable.foldRight((x, acc) -> acc + x, 100, Identity(1)) == 101

    /////////////////////////////////////////////////////////////////////////////
    // Traversable                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool =
        Traversable.traverse(x -> x :: Nil, Identity(1)) == Identity(1) :: Nil

    @Test
    def sequence01(): Bool =
        Traversable.sequence(Identity(1 :: 2 :: Nil)) == Identity(1) :: Identity(2) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // Add                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def add01(): Bool =
        Identity(1) + Identity(2) == Identity(3)

    /////////////////////////////////////////////////////////////////////////////
    // Sub                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sub01(): Bool =
        Identity(3) - Identity(2) == Identity(1)

    /////////////////////////////////////////////////////////////////////////////
    // Mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Bool =
        Identity(3) * Identity(2) == Identity(6)

    /////////////////////////////////////////////////////////////////////////////
    // Div                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def div01(): Bool =
        Identity(6) / Identity(2) == Identity(3)

    /////////////////////////////////////////////////////////////////////////////
    // Neg                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def neg01(): Bool =
        Neg.neg(Identity(1)) == Identity(-1)

    @Test
    def neg02(): Bool =
        Neg.neg(Identity(-1)) == Identity(1)

}
