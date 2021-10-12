/*
 * Copyright 2021 Jakob Schneider Villumsen
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

namespace TestMutDeque {

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool & Impure =
        MutDeque.size(MutDeque.new()) == 0

    @test
    def size02(): Bool & Impure =
        let m = MutDeque.new();

        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);

        MutDeque.size(m) == 4

    @test
    def size03(): Bool & Impure =
        let m = MutDeque.new();

        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);

        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);
        MutDeque.pushBack(1, m);

        MutDeque.size(m) == 8

    @test
    def size04(): Bool & Impure =
        let m = MutDeque.new();

        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);

        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);

        MutDeque.size(m) == 8

    @test
    def size05(): Bool & Impure =
        let m = MutDeque.new();

        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);

        MutDeque.popFront(m);
        MutDeque.popFront(m);

        MutDeque.size(m) == 2


    @test
    def size06(): Bool & Impure =
        let m = MutDeque.new();

        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);
        MutDeque.pushFront(1, m);

        MutDeque.popFront(m);
        MutDeque.popFront(m);
        MutDeque.popFront(m);
        MutDeque.popFront(m);

        MutDeque.size(m) == 0

    // TODO: Add Test for wrap-around (i.e. when back < front)
}