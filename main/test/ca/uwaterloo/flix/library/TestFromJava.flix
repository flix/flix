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

    /////////////////////////////////////////////////////////////////////////////
    // Chain                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def chainFromJava01(): Bool =
        import static java.util.List.of(): ##java.util.List \ {};
        let l = of();
        (FromJava.fromJava(l): Chain[String]) == (Chain.empty(): Chain[String])

    @test
    def chainFromJava02(): Bool =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("one"));
        (FromJava.fromJava(l): Chain[String]) == Chain.singleton("one")

    @test
    def chainFromJava03(): Bool =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("one"), checked_cast("two"));
        (FromJava.fromJava(l): Chain[String]) == List.toChain(List#{"one", "two"})

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def listFromJava01(): Bool =
        import static java.util.List.of(): ##java.util.List \ {};
        let l = of();
        (FromJava.fromJava(l): List[String]) == (Nil: List[String])

    @test
    def listFromJava02(): Bool =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("one"));
        (FromJava.fromJava(l): List[String]) == List#{"one"}

    @test
    def listFromJava03(): Bool =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let l = of(checked_cast("one"), checked_cast("two"));
        (FromJava.fromJava(l): List[String]) == List#{"one", "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFromJava01(): Bool =
        import static java.util.Map.of(): ##java.util.Map \ {};
        let m = of();
        (FromJava.fromJava(m): Map[String, String]) == Map#{}

    @test
    def mapFromJava02(): Bool =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = of(checked_cast("a"), checked_cast("one"));
        (FromJava.fromJava(m): Map[String, String]) == Map#{"a" => "one"}

    @test
    def mapFromJava03(): Bool =
        import static java.util.Map.of(##java.lang.Object, ##java.lang.Object, ##java.lang.Object, ##java.lang.Object): ##java.util.Map \ {};
        let m = of(checked_cast("a"), checked_cast("one"), checked_cast("b"), checked_cast("two"));
        (FromJava.fromJava(m): Map[String, String]) == Map#{"a" => "one", "b" => "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def setFromJava01(): Bool=
        import static java.util.Set.of(): ##java.util.Set \ {};
        let s = of();
        (FromJava.fromJava(s): Set[String]) == Set#{}

    @test
    def setFromJava02(): Bool =
        import static java.util.Set.of(##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("one"));
        (FromJava.fromJava(s): Set[String]) == Set#{"one"}

    @test
    def setFromJava03(): Bool =
        import static java.util.Set.of(##java.lang.Object, ##java.lang.Object): ##java.util.Set \ {};
        let s = of(checked_cast("one"), checked_cast("two"));
        (FromJava.fromJava(s): Set[String]) == Set#{"one", "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Vector                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def vectorFromJava01(): Bool =
        import static java.util.List.of(): ##java.util.List \ {};
        let v = of();
        (FromJava.fromJava(v): Vector[String]) == Vector#{}

    @test
    def vectorFromJava02(): Bool =
        import static java.util.List.of(##java.lang.Object): ##java.util.List \ {};
        let v = of(checked_cast("one"));
        (FromJava.fromJava(v): Vector[String]) == Vector#{"one"}

    @test
    def vectorFromJava03(): Bool =
        import static java.util.List.of(##java.lang.Object, ##java.lang.Object): ##java.util.List \ {};
        let v = of(checked_cast("one"), checked_cast("two"));
        (FromJava.fromJava(v): Vector[String]) == Vector#{"one", "two"}

}
