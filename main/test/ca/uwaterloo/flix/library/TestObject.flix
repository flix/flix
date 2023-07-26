/*
 * Copyright 2021 Matthew Lutze
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

mod TestObject {

    /////////////////////////////////////////////////////////////////////////////
    // isNull                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isNullUnit01(): Bool = not Object.isNull(())

    @test
    def isNullUnit02(): Bool = Object.isNull(unchecked_cast(null as Unit))

    @test
    def isNullBool01(): Bool = not Object.isNull(true)

    @test
    def isNullChar01(): Bool = not Object.isNull('a')

    @test
    def isNullFloat32_01(): Bool = not Object.isNull(123.0f32)

    @test
    def isNullFloat64_01(): Bool = not Object.isNull(123.0f64)

    @test
    def isNullInt8_01(): Bool = not Object.isNull(123i8)

    @test
    def isNullInt16_01(): Bool = not Object.isNull(123i16)

    @test
    def isNullInt32_01(): Bool = not Object.isNull(123i32)

    @test
    def isNullInt64_01(): Bool = not Object.isNull(123i64)

    @test
    def isNullBigInt01(): Bool = not Object.isNull(123ii)

    @test
    def isNullBigInt02(): Bool = Object.isNull(unchecked_cast(null as BigInt))

    @test
    def isNullString01(): Bool = not Object.isNull("hello")

    @test
    def isNullString02(): Bool = Object.isNull(unchecked_cast(null as String))

    @test
    def isNullArray01(): Bool \ IO = not Object.isNull(Array#{} @ Static)

    @test
    def isNullArray02(): Bool = region rc {
        let _ = rc;
        Object.isNull(unchecked_cast(null as Array[Int32, rc]))
    }

    @test
    def isNullOther01(): Bool = not Object.isNull(1 :: 2 :: Nil)

    @test
    def isNullOther02(): Bool = Object.isNull(unchecked_cast(null as List[Int32]))

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toOption01(): Bool = Object.toOption(unchecked_cast(null as String)) == None

    @test
    def toOption02(): Bool = Object.toOption("") == Some("")
}
