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

mod Test.Exp.Fixpoint.Inject {

    enum E[a] with Eq, Order, ToString {
        case E1(a)
        case E2(a, a)
    }

    @test
    def inject01(): Bool = {
        let l = (Vector.empty(): Vector[Bool]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject02(): Bool = {
        let l = (Vector.empty(): Vector[Char]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject03(): Bool = {
        let l = (Vector.empty(): Vector[Float32]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject04(): Bool = {
        let l = (Vector.empty(): Vector[Float64]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject05(): Bool = {
        let l = (Vector.empty(): Vector[BigDecimal]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject06(): Bool = {
        let l = (Vector.empty(): Vector[Int8]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject07(): Bool = {
        let l = (Vector.empty(): Vector[Int16]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject08(): Bool = {
        let l = (Vector.empty(): Vector[Int32]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject09(): Bool = {
        let l = (Vector.empty(): Vector[Int64]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject10(): Bool = {
        let l = (Vector.empty(): Vector[BigInt]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject11(): Bool = {
        let l = (Vector.empty(): Vector[String]);
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject12(): Bool = {
        let l = Vector#{1, 2, 3};
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject13(): Bool = {
        let l = Vector#{"1", "2", "3"};
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject14(): Bool = {
        let l = Vector#{E.E1("1"), E.E1("2"), E.E1("3"), E.E2("1", "2")};
        let pr = inject l into A/1;
        let r = query pr select x from A(x);
        l == r
    }

    @test
    def inject15(): Bool = {
        let l1 = Vector#{1, 2, 3};
        let l2 = Vector#{4, 5, 6};
        let pr = inject l1, l2 into A/1, B/1;
        let r1 = query pr select x from A(x);
        let r2 = query pr select x from B(x);
        l1 == r1 and l2 == r2
    }

    @test
    def inject16(): Bool = {
        let l1 = Vector#{1, 2, 3};
        let l2 = Vector#{'a', 'b', 'c'};
        let pr = inject l1, l2 into A/1, B/1;
        let r1 = query pr select x from A(x);
        let r2 = query pr select x from B(x);
        l1 == r1 and l2 == r2
    }

    @test
    def inject17(): Bool = {
        let l = Vector#{(1, 2), (2, 3), (3, 4)};
        let pr = inject l into A/2;
        let r = query pr select (x, y) from A(x, y);
        l == r
    }

    @test
    def inject18(): Bool = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{4, 5, 6};
        let pr = inject l1, l2 into A/2, B/1;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select x from B(x);
        l1 == r1 and l2 == r2
    }

    @test
    def inject19(): Bool = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{'a', 'b', 'c'};
        let pr = inject l1, l2 into A/2, B/1;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select x from B(x);
        l1 == r1 and l2 == r2
    }

    @test
    def inject20(): Bool = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{('a', 4), ('b', 5), ('c', 6)};
        let pr = inject l1, l2 into A/2, B/2;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select (x, y) from B(x, y);
        l1 == r1 and l2 == r2
    }

    @test
    def inject21(): Bool = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{(4, 5, 6), (5, 6, 7), (6, 7, 8)};
        let pr = inject l1, l2 into A/2, B/3;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select (x, y, z) from B(x, y, z);
        l1 == r1 and l2 == r2
    }

    @test
    def inject22(): Bool = {
        let l1 = Vector#{(1, 2), (2, 3), (3, 4)};
        let l2 = Vector#{(4, 5, 6), (5, 6, 7), (6, 7, 8)};
        let l3 = Set#{1.0f32, 2.0f32, 3.0f32};
        let pr = inject l1, l2, l3 into A/2, B/3, C/1;
        let r1 = query pr select (x, y) from A(x, y);
        let r2 = query pr select (x, y, z) from B(x, y, z);
        let r3 = query pr select x from C(x) |> Vector.toSet;
        l1 == r1 and l2 == r2 and l3 == r3
    }

}
