/*
 *  Copyright 2023 Jakob Schneider Villumsen
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

mod TestRef {

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def new01(): Ref[Int32, Static] \ IO =
        Ref.empty(Static, 1)


    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def get01(): Bool = region rc {
        Assert.eq(1, Ref.get(Ref.empty(rc, 1)))
    }

    @test
    def get02(): Bool = region rc {
        Assert.eq("a", Ref.get(Ref.empty(rc, "a")))
    }

    @test
    def get03(): Bool = region rc {
        Ref.get(Ref.empty(rc, "A")) != "b"
    }

    @test
    def get04(): Bool = region rc {
        Assert.eq("A", Ref.get(Ref.get(Ref.empty(rc, Ref.empty(rc, "A")))))
    }


    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def put01(): Bool = region rc {
        let rf = Ref.empty(rc, 1);
        Ref.put(2, rf);
        Assert.eq(2, Ref.get(rf))
    }

    @test
    def put02(): Bool = region rc {
        let rf = Ref.empty(rc, "a");
        Ref.put("A", rf);
        Assert.eq("A", Ref.get(rf))
    }

    @test
    def put03(): Bool = region rc {
        let rf = Ref.empty(rc, Ref.empty(rc, "a"));
        Ref.put(Ref.empty(rc, "A"), rf);
        Assert.eq("A", Ref.get(Ref.get(rf)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transform                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform01(): Bool = region rc {
        let rf = Ref.empty(rc, 1);
        Ref.transform(x -> x + 1, rf);
        Assert.eq(2, Ref.get(rf))
    }

    @test
    def transform02(): Bool = region rc {
        let rf = Ref.empty(rc, "a");
        Ref.transform(_ -> "A", rf);
        Assert.eq("A", Ref.get(rf))
    }

    @test
    def transform03(): Bool = region rc {
        let rf = Ref.empty(rc, Ref.empty(rc, "a"));
        Ref.transform(rf1 -> { Ref.transform(_ -> "A", rf1) ; rf1 }, rf);
        Assert.eq("A", Ref.get(Ref.get(rf)))
    }
}
