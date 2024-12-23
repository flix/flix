/*
 *  Copyright 2024 Stephen Tetley
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

mod TestFromJava {

    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.util.{List => JList}
    import java.lang.{String => JString}
    import java.math.BigInteger
    import java.math.BigDecimal

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int8FromJava01(): Bool =
        let i = Int8.valueOf(0i8);
        ToFlix.toFlix(i) == 0i8

    @test
    def int8FromJava02(): Bool =
        let i = Int8.valueOf(100i8);
        ToFlix.toFlix(i) == 100i8

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int16FromJava01(): Bool =
        let i = Int16.valueOf(0i16);
        ToFlix.toFlix(i) == 0i16

    @test
    def int16FromJava02(): Bool =
        let i = Int16.valueOf(100i16);
        ToFlix.toFlix(i) == 100i16

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int32FromJava01(): Bool =
        let i = Int32.valueOf(0);
        ToFlix.toFlix(i) == 0

    @test
    def int32FromJava02(): Bool =
        let i = Int32.valueOf(100);
        ToFlix.toFlix(i) == 100

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int64FromJava01(): Bool =
        let i = Int64.valueOf(0i64);
        ToFlix.toFlix(i) == 0i64

    @test
    def int64FromJava02(): Bool =
        let i = Int64.valueOf(100i64);
        ToFlix.toFlix(i) == 100i64

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float32FromJava01(): Bool =
        let d = Float32.valueOf(0.0f32);
        ToFlix.toFlix(d) == 0.0f32

    @test
    def float32FromJava02(): Bool =
        let d = Float32.valueOf(100.0f32);
        ToFlix.toFlix(d) == 100.0f32

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float64FromJava01(): Bool =
        let d = Float64.valueOf(0.0f64);
        ToFlix.toFlix(d) == 0.0f64

    @test
    def float64FromJava02(): Bool =
        let d = Float64.valueOf(100.0f64);
        ToFlix.toFlix(d) == 100.0f64

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigIntFromJava01(): Bool =
        let i = unsafe BigInteger.valueOf(0i64);
        ToFlix.toFlix(i) == 0ii

    @test
    def bigIntFromJava02(): Bool =
        let i = unsafe BigInteger.valueOf(100i64);
        ToFlix.toFlix(i) == 100ii

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigDecimalFromJava01(): Bool =
        let d = unsafe BigDecimal.valueOf(0i64);
        ToFlix.toFlix(d) == 0.0ff

    @test
    def bigDecimalFromJava02(): Bool =
        let d = unsafe BigDecimal.valueOf(100i64);
        ToFlix.toFlix(d) == 100.0ff

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def charFromJava01(): Bool =
        let c = Char.valueOf('0');
        ToFlix.toFlix(c) == '0'

    @test
    def charFromJava02(): Bool =
        let c = Char.valueOf('a');
        ToFlix.toFlix(c) == 'a'

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stringFromJava01(): Bool =
        let s = unsafe JString.valueOf(true);
        ToFlix.toFlix(s) == "true"

    @test
    def stringFromJava02(): Bool =
        let s = unsafe JString.valueOf(false);
        ToFlix.toFlix(s) == "false"

    /////////////////////////////////////////////////////////////////////////////
    // Chain                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def chainFromJava01(): Bool =
        let l = unsafe JList.of();
        (ToFlix.toFlix(l): Chain[String]) == (Chain.empty(): Chain[String])

    @test
    def chainFromJava02(): Bool =
        let l = unsafe JList.of("one");
        (ToFlix.toFlix(l): Chain[String]) == Chain.singleton("one")

    @test
    def chainFromJava03(): Bool =
        let l = unsafe JList.of("one", "two");
        (ToFlix.toFlix(l): Chain[String]) == List.toChain(List#{"one", "two"})

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def listFromJava01(): Bool =
        let l = unsafe JList.of();
        (ToFlix.toFlix(l): List[String]) == (Nil: List[String])

    @test
    def listFromJava02(): Bool =
        let l = unsafe JList.of("one");
        (ToFlix.toFlix(l): List[String]) == List#{"one"}

    @test
    def listFromJava03(): Bool =
        let l = unsafe JList.of("one", "two");
        (ToFlix.toFlix(l): List[String]) == List#{"one", "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFromJava01(): Bool =
        let m = unsafe JMap.of();
        (ToFlix.toFlix(m): Map[String, String]) == Map#{}

    @test
    def mapFromJava02(): Bool =
        let m = unsafe JMap.of("a", "one");
        (ToFlix.toFlix(m): Map[String, String]) == Map#{"a" => "one"}

    @test
    def mapFromJava03(): Bool =
        let m = unsafe JMap.of("a", "one", "b", "two");
        (ToFlix.toFlix(m): Map[String, String]) == Map#{"a" => "one", "b" => "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def setFromJava01(): Bool=
        let s = unsafe JSet.of();
        (ToFlix.toFlix(s): Set[String]) == Set#{}

    @test
    def setFromJava02(): Bool =
        let s = unsafe JSet.of("one");
        (ToFlix.toFlix(s): Set[String]) == Set#{"one"}

    @test
    def setFromJava03(): Bool =
        let s = unsafe JSet.of("one", "two");
        (ToFlix.toFlix(s): Set[String]) == Set#{"one", "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Vector                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def vectorFromJava01(): Bool =
        let v = unsafe JList.of();
        (ToFlix.toFlix(v): Vector[String]) == Vector#{}

    @test
    def vectorFromJava02(): Bool =
        let v = unsafe JList.of("one");
        (ToFlix.toFlix(v): Vector[String]) == Vector#{"one"}

    @test
    def vectorFromJava03(): Bool =
        let v = unsafe JList.of("one", "two");
        (ToFlix.toFlix(v): Vector[String]) == Vector#{"one", "two"}

}
