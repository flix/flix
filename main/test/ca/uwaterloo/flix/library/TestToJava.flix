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

mod TestToJava {

    import java.util.{List => JList}
    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.lang.{Object => JObject}

    /////////////////////////////////////////////////////////////////////////////
    // toJavaObject                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toJavaObject01(): Bool =
        let x = ToJava.toJavaObject(0);
        typematch x {
            case _: JObject => true
            case _: _ => false
        }

    @test
    def toJavaObject02(): Bool =
        let x = ToJava.toJavaObject("hello world!");
        typematch x {
            case _: JObject => true
            case _: _ => false
        }

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int8ToJava01(): Bool =
        let i = ToJava.toJava(0i8);
        Int8.byteValue(i) == 0i8

    @test
    def int8ToJava02(): Bool =
        let i = ToJava.toJava(100i8);
        Int8.byteValue(i) == 100i8

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int16ToJava01(): Bool =
        let i = ToJava.toJava(0i16);
        Int16.shortValue(i) == 0i16

    @test
    def int16ToJava02(): Bool =
        let i = ToJava.toJava(100i16);
        Int16.shortValue(i) == 100i16

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int32ToJava01(): Bool =
        let i = ToJava.toJava(0);
        Int32.intValue(i) == 0

    @test
    def int32ToJava02(): Bool =
        let i = ToJava.toJava(100);
        Int32.intValue(i) == 100

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int64ToJava01(): Bool =
        let i = ToJava.toJava(0i64);
        Int64.longValue(i) == 0i64

    @test
    def int64ToJava02(): Bool =
        let i = ToJava.toJava(100i64);
        Int64.longValue(i) == 100i64

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float32ToJava01(): Bool =
        let d = ToJava.toJava(0.0f32);
        Float32.floatValue(d) == 0.0f32

    @test
    def float32ToJava02(): Bool =
        let d = ToJava.toJava(100.0f32);
        Float32.floatValue(d) == 100.0f32

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float64ToJava01(): Bool =
        let d = ToJava.toJava(0.0f64);
        Float64.doubleValue(d) == 0.0f64

    @test
    def float64ToJava02(): Bool =
        let d = ToJava.toJava(100.0f64);
        Float64.doubleValue(d) == 100.0f64

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigIntToJava01(): Bool =
        let i = ToJava.toJava(0ii);
        i == 0ii

    @test
    def bigIntToJava02(): Bool =
        let i = ToJava.toJava(100ii);
        i == 100ii

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigDecimalToJava01(): Bool =
        let d = ToJava.toJava(0.0ff);
        d == 0.0ff

    @test
    def bigDecimalToJava02(): Bool =
        let d = ToJava.toJava(100.0ff);
        d == 100.0ff

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def charToJava01(): Bool =
        let c = ToJava.toJava('0');
        Char.charValue(c) == '0'

    @test
    def charToJava02(): Bool =
        let c = ToJava.toJava('a');
        Char.charValue(c) == 'a'

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stringToJava01(): Bool =
        let s = ToJava.toJava("");
        s == ""

    @test
    def stringToJava02(): Bool =
        let s = ToJava.toJava("hello world!");
        s == "hello world!"

    /////////////////////////////////////////////////////////////////////////////
    // Chain                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def chainToJava01(): Bool \ IO =
        let l: JList = ToJava.toJava((Chain.empty() : Chain[String]));
        unsafe JList.of().equals(l)

    @test
    def chainToJava02(): Bool \ IO =
        let l: JList = ToJava.toJava(Chain.singleton("one"));
        unsafe JList.of("one").equals(l)

    @test
    def chainToJava03(): Bool \ IO =
        let l: JList = ToJava.toJava(Chain.cons("one", Chain.singleton("two")));
        unsafe JList.of("one", "two").equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def listToJava01(): Bool \ IO =
        let l: JList = ToJava.toJava((Nil : List[String]));
        unsafe JList.of().equals(l)

    @test
    def listToJava02(): Bool \ IO =
        let l: JList = ToJava.toJava(List#{"one"});
        unsafe JList.of("one").equals(l)

    @test
    def listToJava03(): Bool \ IO =
        let l: JList = ToJava.toJava(List#{"one", "two"});
        unsafe JList.of("one", "two").equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapToJava01(): Bool \ IO =
        let m: JMap = ToJava.toJava((Map#{} : Map[String, String]));
        unsafe JMap.of().equals(m)

    @test
    def mapToJava02(): Bool \ IO =
        let m: JMap = ToJava.toJava(Map#{"a" => "one"});
        unsafe JMap.of("a", "one").equals(m)

    @test
    def mapToJava03(): Bool \ IO =
        let m: JMap = ToJava.toJava(Map#{"a" => "one", "b" => "two"});
        unsafe JMap.of("a", "one", "b", "two").equals(m)

    /////////////////////////////////////////////////////////////////////////////
    // Nec                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def necToJava01(): Bool \ IO =
        let l: JList = ToJava.toJava(Nec.singleton("one"));
        unsafe JList.of("one").equals(l)

    @test
    def necToJava02(): Bool \ IO =
        let l: JList = ToJava.toJava(Nec.cons("one", Nec.singleton("two")));
        unsafe JList.of("one", "two").equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // Nel                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nelToJava01(): Bool \ IO =
        let l: JList = ToJava.toJava(Nel.singleton("one"));
        unsafe JList.of("one").equals(l)

    @test
    def nelToJava02(): Bool \ IO =
        let l: JList = ToJava.toJava(Nel.cons("one", Nel.singleton("two")));
        unsafe JList.of("one", "two").equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def setToJava01(): Bool \ IO =
        let s: JSet = ToJava.toJava((Set#{} : Set[String]));
        unsafe JSet.of().equals(s)

    @test
    def setToJava02(): Bool \ IO =
        let s: JSet = ToJava.toJava(Set#{"one"});
        unsafe JSet.of("one").equals(s)

    @test
    def setToJava03(): Bool \ IO =
        let s: JSet = ToJava.toJava(Set#{"one", "two"});
        unsafe JSet.of("one", "two").equals(s)

    /////////////////////////////////////////////////////////////////////////////
    // Vector                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def vectorToJava01(): Bool \ IO =
        let v: JList = ToJava.toJava((Vector#{} : Vector[String]));
        unsafe JList.of().equals(v)

    @test
    def vectorToJava02(): Bool \ IO =
        let v: JList = ToJava.toJava(Vector#{"one"});
        unsafe JList.of("one").equals(v)

    @test
    def vectorToJava03(): Bool \ IO =
        let v: JList = ToJava.toJava(Vector#{"one", "two"});
        unsafe JList.of("one", "two").equals(v)

}
