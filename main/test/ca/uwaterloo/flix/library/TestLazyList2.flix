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

///
/// Additional Tests for LazyList.
///
namespace TestLazyList2 {

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ENil) == None

    @test
    def findMap02(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ENil)) == None

    @test
    def findMap03(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LCons(1, lazy ENil)) == None

    @test
    def findMap04(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, LList(lazy ENil))) == None

    @test
    def findMap05(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LCons(1, lazy LList(lazy ENil))) == None

    @test
    def findMap06(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == None

    @test
    def findMap07(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ECons(2, LList(lazy ENil)))) == Some(1)

    @test
    def findMap08(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LCons(2, lazy ENil)) == Some(1)

    @test
    def findMap09(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(2, LList(lazy ENil))) == Some(1)

    @test
    def findMap10(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ECons(2, ENil))) == Some(1)

	@test
	def findMap11(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ECons(1, ECons(3, ENil)))) == None

	@test
	def findMap12(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy LCons(1, lazy LCons(3, lazy LList(lazy ENil))))) == None

	@test
	def findMap13(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ECons(1, LCons(3, lazy LList(lazy ENil))))) == None

	@test
	def findMap14(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, LList(lazy ENil)))) == None

	@test
	def findMap15(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == Some(2)

	@test
	def findMap16(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == Some(3)

	@test
	def findMap17(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == Some(4)

    @test
	def findMap18(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapPure01(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ENil) == ENil

    @test
    def filterMapPure02(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ENil)) == ENil

    @test
    def filterMapPure03(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LCons(1, lazy ENil)) == ENil

    @test
    def filterMapPure04(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, LList(lazy ENil))) == ENil

    @test
    def filterMapPure05(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LCons(1, lazy LList(lazy ENil))) == ENil

    @test
    def filterMapPure06(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == ENil

    @test
    def filterMapPure07(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ECons(2, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def filterMapPure08(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LCons(2, lazy ENil)) == ECons(1, ENil)

    @test
    def filterMapPure09(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(2, LList(lazy ENil))) == ECons(1, ENil)

    @test
    def filterMapPure10(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ECons(2, ENil))) == ECons(1, ENil)

	@test
	def filterMapPure11(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ECons(1, ECons(3, ENil)))) == ENil

	@test
	def filterMapPure12(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy LCons(1, lazy LCons(3, lazy LList(lazy ENil))))) == ENil

	@test
	def filterMapPure13(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, LList(lazy ECons(1, LCons(3, lazy LList(lazy ENil))))) == ENil

	@test
	def filterMapPure14(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, LList(lazy ENil)))) == ENil

	@test
	def filterMapPure15(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapPure16(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapPure17(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @test
	def filterMapPure18(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapImpure01(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ENil) == ENil

    @test
    def filterMapImpure02(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LList(lazy ENil)) == ENil

    @test
    def filterMapImpure03(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LCons(1, lazy ENil)) == ENil

    @test
    def filterMapImpure04(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(1, LList(lazy ENil))) == ENil

    @test
    def filterMapImpure05(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LCons(1, lazy LList(lazy ENil))) == ENil

    @test
    def filterMapImpure06(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ENil)) == ENil

    @test
    def filterMapImpure07(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LList(lazy ECons(2, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def filterMapImpure08(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LCons(2, lazy ENil)) == ECons(1, ENil)

    @test
    def filterMapImpure09(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(2, LList(lazy ENil))) == ECons(1, ENil)

    @test
    def filterMapImpure10(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LList(lazy ECons(2, ENil))) == ECons(1, ENil)

	@test
	def filterMapImpure11(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LList(lazy ECons(1, ECons(3, ENil)))) == ENil

	@test
	def filterMapImpure12(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LList(lazy LCons(1, lazy LCons(3, lazy LList(lazy ENil))))) == ENil

	@test
	def filterMapImpure13(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, LList(lazy ECons(1, LCons(3, lazy LList(lazy ENil))))) == ENil

	@test
	def filterMapImpure14(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ECons(3, LList(lazy ENil)))) == ENil

	@test
	def filterMapImpure15(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapImpure16(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapImpure17(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @test
	def filterMapImpure18(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap filterMap                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFilterMap01(): Bool =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None) |>
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap02(): Bool & Impure =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure) |>
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap03(): Bool & Impure =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None) |>
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3) else None as & Impure) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap04(): Bool & Impure =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure) |>
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3 as & Impure) else None) == ECons(6, ECons(9, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap filterMap fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.filterMap(x -> { l := "a" :: deref l; Some(x) }) |>
        LazyList.filterMap(x -> { l := "b" :: deref l; Some(x) });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.filterMap(x -> { l := "a" :: deref l; Some(x) } as & Pure) |>
        LazyList.filterMap(x -> { l := "b" :: deref l; Some(x) } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        LazyList.toString(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString02(): Bool =
        LazyList.toString(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString03(): Bool =
        LazyList.toString(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString04(): Bool =
        LazyList.toString(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString05(): Bool =
        LazyList.toString(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString06(): Bool =
        LazyList.toString(LCons(1, lazy ECons(2, ECons(3, LList(lazy ENil))))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString07(): Bool =
        LazyList.toString(ECons(1, ECons(2, LList(lazy LCons(3, lazy ENil))))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString08(): Bool =
        LazyList.toString(ECons(1, LCons(2, lazy LCons(3, lazy LList(lazy ENil))))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString09(): Bool =
        LazyList.toString(ECons(1, ECons(2, ECons(3, ENil)))) == "1 :: 2 :: 3 :: ENil"

}