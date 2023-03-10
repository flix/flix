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
        Ref.new(Static, 1)


    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def get01(): Bool = region rh {
        Assert.eq(1, Ref.get(Ref.new(rh, 1)))
    }

    @test
    def get02(): Bool = region rh {
        Assert.eq("a", Ref.get(Ref.new(rh, "a")))
    }

    @test
    def get03(): Bool = region rh {
        Ref.get(Ref.new(rh, "A")) != "b"
    }

}
