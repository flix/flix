/*
 *  Copyright 2023 Stephen Tetley
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

mod TestGradedApplicative {

    use GradedFunctor.{<$>, <$$>, <$, $>}
    use GradedApplicative.{point, ap, map2, map3, map4, map5, product, productLeft, productRight,
        product3, product4, product5, <*>, <**>, *>, <*}

    use TestGradedApplicative.Int32Reader
    use TestGradedApplicative.Int32Reader.{Int32Reader}

    pub enum Int32Reader[ef: Bool, a: Type](Int32 -> a \ ef)

    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def runReader(ma: Int32Reader[ef, a], i: Int32): a \ ef =
        let Int32Reader(ma1) = ma;
        ma1(i)

    ///
    /// Return the current environemnt.
    ///
    def ask(): Int32Reader[ef, Int32] =
        Int32Reader(i -> checked_ecast(i))

    ///
    /// Use an array computation which changes the effect type (i.e. the "grade") of the Int32Reader.
    ///
    def genArray(rc: Region[r], n: Int32): Int32Reader[ef and r, Array[Int32, r]] =
        Int32Reader(i ->
            checked_ecast(Array.repeat(rc, n, i))
        )

    instance GradedFunctor[Int32Reader] {
        pub def map(f: a -> b \ ef1, ma: Int32Reader[ef, a]): Int32Reader[ef and ef1, b] =
            Int32Reader(i ->
                let a = runReader(unchecked_cast(ma as Int32Reader[ef and ef1, a]), i);
                f(a)
            )
    }

    instance GradedApplicative[Int32Reader] {
        pub def point(x: a): Int32Reader[ef, a] =
            Int32Reader(_ -> checked_ecast(x))

        pub def ap(mf: Int32Reader[ef1, a -> b \ ef2], ma: Int32Reader[ef, a]): Int32Reader[ef and ef1 and ef2, b] =
            Int32Reader(i ->
                let f = runReader(unchecked_cast(mf as Int32Reader[ef and ef1 and ef2, a -> b \ ef2]), i);
                let a = runReader(unchecked_cast(ma as Int32Reader[ef and ef1 and ef2, a]), i);
                f(a)
            )
    }

    /////////////////////////////////////////////////////////////////////////////
    // point                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def point01(): Bool =
        runReader(point(1), 0) == 1

    @test
    def point02(): Bool =
        let noneI32: Option[Int32] = None;
        runReader(point(noneI32), 0) == None

    @test
    def point03(): Bool =
        runReader(point(Some(1)), 1) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool =
        runReader(ap(point(x -> x + 1), point(2)), 1) == 3

    @test
    def ap02(): Bool =
        runReader(ap(point(x -> Some(x)), point(2)), 1) == Some(2)

    @test
    def ap03(): Bool =
        runReader(ap(point(x -> Some(x)), ask()), 1) == Some(1)

    @test
    def ap04(): Bool = region rc {
        runReader(ap(point(x -> Array.length(x)), genArray(rc, 3)), 1) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // map2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map201(): Bool =
        runReader(map2((x, y) -> if (x > 0) Some(y) else None, point(1), point(2)), 1) == Some(2)

    @test
    def map202(): Bool =
        runReader(map2((x, y) -> if (x > 0) Some(y) else None, ask(), point(2)), 1) == Some(2)

    @test
    def map203(): Bool =
        runReader(map2((x, y) -> if (x > 0) Some(y) else None, point(0), point(2)), 1) == None

    @test
    def map204(): Bool = region rc {
        runReader(map2((x, arr) -> x + Array.length(arr), point(1), genArray(rc, 3)), 1) == 4
    }

    /////////////////////////////////////////////////////////////////////////////
    // map3                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map301(): Bool =
        runReader(map3((x, y, z) -> if (x > 0) Some(y + z) else None, point(1), point(2), point(3)), 1) == Some(5)

    @test
    def map302(): Bool =
        runReader(map3((x, y, z) -> if (x > 0) Some(y + z) else None, ask(), point(2), point(3)), 1) == Some(5)

    @test
    def map303(): Bool =
        runReader(map3((x, y, z) -> if (x > 0) Some(y + z) else None, point(0), point(2), point(3)), 1) == None

    @test
    def map304(): Bool = region rc {
        runReader(map3((x, y, arr) -> x + y + Array.length(arr), point(1), point(2), genArray(rc, 3)), 1) == 6
    }

    /////////////////////////////////////////////////////////////////////////////
    // map4                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map401(): Bool =
        runReader(map4((v, x, y, z) -> if (v > 0) Some(x + y + z) else None, point(1), point(2), point(3), point(4)), 1) == Some(9)

    @test
    def map402(): Bool =
        runReader(map4((v, x, y, z) -> if (v > 0) Some(x + y + z) else None, ask(), point(2), point(3), point(4)), 1) == Some(9)

    @test
    def map403(): Bool =
        runReader(map4((v, x, y, z) -> if (v > 0) Some(x + y + z) else None, point(0), point(2), point(3), point(4)), 1) == None

    @test
    def map404(): Bool = region rc {
        runReader(map4((x, y, z, arr) -> x + y + z + Array.length(arr), point(1), point(2), point(3), genArray(rc, 3)), 1) == 9
    }

    /////////////////////////////////////////////////////////////////////////////
    // map5                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map501(): Bool =
        runReader(map5((u, v, x, y, z) -> if (u > 0) Some(v + x + y + z) else None, point(1), point(2), point(3), point(4), point(5)), 1) == Some(14)

    @test
    def map502(): Bool =
        runReader(map5((u, v, x, y, z) -> if (u > 0) Some(v + x + y + z) else None, ask(), point(2), point(3), point(4), point(5)), 1) == Some(14)

    @test
    def map503(): Bool =
        runReader(map5((u, v, x, y, z) -> if (u > 0) Some(v + x + y + z) else None, point(0), point(2), point(3), point(4), point(5)), 1) == None

    @test
    def map405(): Bool = region rc {
        runReader(map5((u, x, y, z, arr) -> u + x + y + z + Array.length(arr), point(1), point(2), point(3), point(4), genArray(rc, 3)), 1) == 13
    }

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool =
        runReader(product(ask(), point(2)), 1) == (1, 2)

    @test
    def product02(): Bool =
        runReader(product(ask(), point(Some(2))), 1) == (1, Some(2))

    @test
    def product03(): Bool =
        runReader(product(point(Some(2)), ask()), 1) == (Some(2), 1)

    @test
    def product04(): Bool = region rc {
        runReader(product(genArray(rc, 3) <$$> Array.toList, ask()), 1) == (List#{1, 1, 1}, 1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // productLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productLeft01(): Bool =
        runReader(productLeft(ask(), point(2)), 1) == 1

    @test
    def productLeft02(): Bool =
        runReader(productLeft(ask(), point(Some(2))), 1) == 1

    @test
    def productLeft03(): Bool =
        runReader(productLeft(point(Some(2)), ask()), 1) == Some(2)

    @test
    def productLeft04(): Bool = region rc {
        runReader(productLeft(point(Some(2)), genArray(rc, 3)), 1) == Some(2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // productRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productRight01(): Bool =
        runReader(productRight(ask(), point(2)), 1) == 2

    @test
    def productRight02(): Bool =
        runReader(productRight(ask(), point(Some(2))), 1) == Some(2)

    @test
    def productRight03(): Bool =
        runReader(productRight(point(Some(2)), ask()), 1) == 1

    @test
    def productRight04(): Bool = region rc {
        runReader(productRight(genArray(rc, 3), ask()), 1) == 1
    }

    /////////////////////////////////////////////////////////////////////////////
    // product3                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product301(): Bool =
        runReader(product3(ask(), point(2), point(3)), 1) == (1, 2, 3)

    @test
    def product302(): Bool =
        runReader(product3(ask(), point(Some(2)), point(3)), 1) == (1, Some(2), 3)

    @test
    def product303(): Bool =
        runReader(product3(point(Some(2)), ask(), point(3)), 1) == (Some(2), 1, 3)

    @test
    def product304(): Bool = region rc {
        runReader(product3(genArray(rc, 3) <$$> Array.toList, ask(), point(3)), 1) == (List#{1, 1, 1}, 1, 3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // product4                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product401(): Bool =
        runReader(product4(ask(), point(2), point(3), point(Some(4))), 1) == (1, 2, 3, Some(4))

    @test
    def product402(): Bool =
        runReader(product4(ask(), point(Some(2)), point(3), point(Some(4))), 1) == (1, Some(2), 3, Some(4))

    @test
    def product403(): Bool =
        runReader(product4(point(Some(2)), ask(), point(3), point(Some(4))), 1) == (Some(2), 1, 3, Some(4))

    @test
    def product404(): Bool = region rc {
        runReader(product4(genArray(rc, 3) <$$> Array.toList, ask(), point(3), point(Some(4))), 1) == (List#{1, 1, 1}, 1, 3, Some(4))
    }

    /////////////////////////////////////////////////////////////////////////////
    // product5                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product501(): Bool =
        runReader(product5(ask(), point(2), point(3), point(Some(4)), point("five")), 1) == (1, 2, 3, Some(4), "five")

    @test
    def product502(): Bool =
        runReader(product5(ask(), point(Some(2)), point(3), point(Some(4)), point("five")), 1) == (1, Some(2), 3, Some(4), "five")

    @test
    def product503(): Bool =
        runReader(product5(point(Some(2)), ask(), point(3), point(Some(4)), point("five")), 1) == (Some(2), 1, 3, Some(4), "five")

    @test
    def product504(): Bool = region rc {
        runReader(product5(genArray(rc, 3) <$$> Array.toList, ask(), point(3), point(Some(4)), point("five")), 1) == (List#{1, 1, 1}, 1, 3, Some(4), "five")
    }

    /////////////////////////////////////////////////////////////////////////////
    // symAp                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symAp01(): Bool =
        runReader(point(x -> x + 1) <*> point(2), 1) == 3

    @test
    def symAp02(): Bool =
        runReader(point(x -> Some(x)) <*> point(2), 1) == Some(2)

    @test
    def symAp03(): Bool =
        runReader(point(x -> Some(x)) <*> ask(), 1) == Some(1)

    @test
    def symAp04(): Bool = region rc {
        runReader(point(x -> Array.length(x)) <*> genArray(rc, 3), 1) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // symFlippedAp                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symFlippedAp01(): Bool =
        runReader(point(2) <**> point(x -> x + 1), 1) == 3

    @test
    def symFlippedAp02(): Bool =
        runReader(point(2) <**> point(x -> Some(x)), 1) == Some(2)

    @test
    def symFlippedAp03(): Bool =
        runReader(ask() <**> point(x -> Some(x)), 1) == Some(1)


    @test
    def symFlippedAp04(): Bool = region rc {
        runReader(genArray(rc, 3) <**> point(x -> Array.length(x)), 1) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // symProductLeft                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symProductLeft01(): Bool =
        runReader(ask() <*  point(2), 1) == 1

    @test
    def symProductLeft02(): Bool =
        runReader(ask() <* point(Some(2)), 1) == 1

    @test
    def symProductLeft03(): Bool =
        runReader(point(Some(2)) <* ask(), 1) == Some(2)

    @test
    def symProductLeft04(): Bool = region rc {
        runReader(point(Some(2)) <* genArray(rc, 3), 1) == Some(2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // symProductRight                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symProductRight01(): Bool =
        runReader(ask() *> point(2), 1) == 2

    @test
    def symProductRight02(): Bool =
        runReader(ask() *> point(Some(2)), 1) == Some(2)

    @test
    def symProductRight03(): Bool =
        runReader(point(Some(2)) *> ask(), 1) == 1

    @test
    def symProductRight04(): Bool = region rc {
        runReader(genArray(rc, 3) *> ask(), 1) == 1
    }

}
