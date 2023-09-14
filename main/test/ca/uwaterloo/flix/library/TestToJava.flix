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

    /////////////////////////////////////////////////////////////////////////////
    // Chain                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def chainToJava01(): Bool \ IO =
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(Chain.empty() : Chain[String]);
        equals(of(), checked_cast(l))

    @Test
    def chainToJava02(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(Chain.singleton("one"));
        equals(of(checked_cast("one")), checked_cast(l))

    @Test
    def chainToJava03(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(Chain.cons("one", Chain.singleton("two")));
        equals(of(checked_cast("one"), checked_cast("two")), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def listToJava01(): Bool \ IO =
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(Nil : List[String]);
        equals(of(), checked_cast(l))

    @Test
    def listToJava02(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(List#{"one"});
        equals(of(checked_cast("one")), checked_cast(l))

    @Test
    def listToJava03(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(List#{"one", "two"});
        equals(of(checked_cast("one"), checked_cast("two")), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapToJava01(): Bool \ IO =
        import static java.util.Map.of(): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m: ##java.util.Map = ToJava.toJava(Map#{} : Map[String, String]);
        equals(of(), checked_cast(m))

    @Test
    def mapToJava02(): Bool \ IO =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m: ##java.util.Map = ToJava.toJava(Map#{"a" => "one"});
        equals(of(checked_cast("a"), checked_cast("one")), checked_cast(m))

    @Test
    def mapToJava03(): Bool \ IO =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        import java.util.Map.equals(##java.lang.Object): Bool \ {};
        let m: ##java.util.Map = ToJava.toJava(Map#{"a" => "one", "b" => "two"});
        equals(of(checked_cast("a"), checked_cast("one"), checked_cast("b"), checked_cast("two")), checked_cast(m))

    /////////////////////////////////////////////////////////////////////////////
    // Nec                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def necToJava01(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(Nec.singleton("one"));
        equals(of(checked_cast("one")), checked_cast(l))

    @Test
    def necToJava02(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(Nec.cons("one", Nec.singleton("two")));
        equals(of(checked_cast("one"), checked_cast("two")), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // Nel                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nelToJava01(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(Nel.singleton("one"));
        equals(of(checked_cast("one")), checked_cast(l))

    @Test
    def nelToJava02(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let l: ##java.util.List = ToJava.toJava(Nel.cons("one", Nel.singleton("two")));
        equals(of(checked_cast("one"), checked_cast("two")), checked_cast(l))

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool \ IO =
        import static java.util.Set.of(): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s: ##java.util.Set = ToJava.toJava(Set#{} : Set[String]);
        equals(of(), checked_cast(s))

    @Test
    def toSet02(): Bool \ IO =
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s: ##java.util.Set = ToJava.toJava(Set#{"one"});
        equals(of(checked_cast("one")), checked_cast(s))

    @Test
    def toSet03(): Bool \ IO =
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        import java.util.Set.equals(##java.lang.Object): Bool \ {};
        let s: ##java.util.Set = ToJava.toJava(Set#{"one", "two"});
        equals(of(checked_cast("one"), checked_cast("two")), checked_cast(s))

    /////////////////////////////////////////////////////////////////////////////
    // Vector                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def vectorToJava01(): Bool \ IO =
        import static java.util.List.of(): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let v: ##java.util.List = ToJava.toJava(Vector#{} : Vector[String]);
        equals(of(), checked_cast(v))

    @Test
    def vectorToJava02(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let v: ##java.util.List = ToJava.toJava(Vector#{"one"});
        equals(of(checked_cast("one")), checked_cast(v))

    @Test
    def vectorToJava03(): Bool \ IO =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        import java.util.List.equals(##java.lang.Object): Bool \ {};
        let v: ##java.util.List = ToJava.toJava(Vector#{"one", "two"});
        equals(of(checked_cast("one"), checked_cast("two")), checked_cast(v))

}
