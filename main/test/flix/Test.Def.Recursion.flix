/*
 * Copyright 2025 Jakob Schneider Villumsen
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

mod Test.Def.Recursion {

    use Lizt.{Null, Conz};
    use Opt.{Empty, El};

    enum Lizt[a] {
        case Null
        case Conz(a, Lizt[a])
    }

    enum Opt[a] with Eq, Order, ToString {
        case Empty
        case El(a)
    }

    def emptyList(): Lizt[Int32] = Null

    def list1(x: a): Lizt[a] = Conz(x, Null)

    def list2(x: a): Lizt[a] = Conz(x, Conz(x, Null))

    def list3(x: a): Lizt[a] = Conz(x, Conz(x, Conz(x, Null)))

    def simple(l: Lizt[a]): Opt[a] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(x)
        case Conz(_, xs)   => simple(xs)
    }

    @test
    def simple01(): Bool = {
        Assert.eq(Empty, simple(emptyList()))
    }

    @test
    def simple02(): Bool = {
        Assert.eq(El(1), simple(list1(1)))
    }

    @test
    def simple03(): Bool = {
        Assert.eq(El(1), simple(list2(1)))
    }

    @test
    def simple04(): Bool = {
        Assert.eq(El(1), simple(list3(1)))
    }

}
