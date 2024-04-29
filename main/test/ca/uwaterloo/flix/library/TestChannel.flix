/*
 * Copyright 2019 Magnus Madsen
 * Copyright 2021 Jonathan Lindegaard Starup
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

 mod TestChannel {

    use Concurrent.Channel.Mpmc
    use Concurrent.Channel.unsafeGetAndUnlock
    use Concurrent.Channel.get
    use Concurrent.Channel.mpmcAdmin
    use Concurrent.Channel.newChannel
    use Concurrent.Channel.put
    use Concurrent.Channel.selectFrom

    @test
    def testNew01(): Mpmc[Int32, Static] \ IO = newChannel(0)

    @test
    def testNew02(): Mpmc[Result[String, a], Static] \ IO = newChannel(32)

    @test
    def testGetPut01(): Bool \ IO =
        let c = newChannel(1);
        put(true, c);
        get(c)

    @test
    def testGetPut02(): Bool \ IO =
        let c = newChannel(1);
        put(123, c);
        get(c) == 123

    @test
    def testGetPut03(): Bool \ IO =
        let c = newChannel(1);
        put("Hello World!", c);
        get(c) == "Hello World!"

    @test
    def testSelect01(): Bool \ IO =
        let c = newChannel(1);
        put(2, c);
        match selectFrom(mpmcAdmin(c) :: Nil, true) {
            case (0, locks) =>
                let i = unsafeGetAndUnlock(c, locks);
                i == 2
            case _ => unreachable!()
        }

    @test
    def testSelect02(): Bool \ IO =
        let c = newChannel(1);
        match selectFrom(mpmcAdmin(c) :: Nil, false) {
            case (0, locks) =>
                let _ = unsafeGetAndUnlock(c, locks);
                false
            case (-1, _) =>
                true
            case _ => unreachable!()
        }

    @test
    def testSelect03(): Bool \ IO =
        let c1: Mpmc[Int32, Static] = newChannel(0);
        let c2: Mpmc[String, _] = newChannel(10);

        put("hey", c2);

        match selectFrom(mpmcAdmin(c1) :: mpmcAdmin(c2) :: Nil, true) {
            case (0, locks) =>
                let _ = unsafeGetAndUnlock(c1, locks);
                bug!("The channel should be empty")
            case (1, locks) =>
                let s = unsafeGetAndUnlock(c2, locks);
                s == "hey"
            case _ => unreachable!()
        }

}
