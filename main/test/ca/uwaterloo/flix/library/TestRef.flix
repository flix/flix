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

    @Test
    def to01(): Ref[Int32, Static] \ IO =
        Ref.fresh(Static, 1)

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def get01(): Bool = region rc {
        Assert.eq(1, Ref.get(Ref.fresh(rc, 1)))
    }

    @Test
    def get02(): Bool = region rc {
        Assert.eq("a", Ref.get(Ref.fresh(rc, "a")))
    }

    @Test
    def get03(): Bool = region rc {
        Ref.get(Ref.fresh(rc, "A")) != "b"
    }

    @Test
    def get04(): Bool = region rc {
        Assert.eq("A", Ref.get(Ref.get(Ref.fresh(rc, Ref.fresh(rc, "A")))))
    }

    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def put01(): Bool = region rc {
        let rf = Ref.fresh(rc, 1);
        Ref.put(2, rf);
        Assert.eq(2, Ref.get(rf))
    }

    @Test
    def put02(): Bool = region rc {
        let rf = Ref.fresh(rc, "a");
        Ref.put("A", rf);
        Assert.eq("A", Ref.get(rf))
    }

    @Test
    def put03(): Bool = region rc {
        let rf = Ref.fresh(rc, Ref.fresh(rc, "a"));
        Ref.put(Ref.fresh(rc, "A"), rf);
        Assert.eq("A", Ref.get(Ref.get(rf)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transform01(): Bool = region rc {
        let rf = Ref.fresh(rc, 1);
        Ref.transform(x -> x + 1, rf);
        Assert.eq(2, Ref.get(rf))
    }

    @Test
    def transform02(): Bool = region rc {
        let rf = Ref.fresh(rc, "a");
        Ref.transform(_ -> "A", rf);
        Assert.eq("A", Ref.get(rf))
    }

    @Test
    def transform03(): Bool = region rc {
        let rf = Ref.fresh(rc, Ref.fresh(rc, "a"));
        Ref.transform(rf1 -> { Ref.transform(_ -> "A", rf1) ; rf1 }, rf);
        Assert.eq("A", Ref.get(Ref.get(rf)))
    }
}

