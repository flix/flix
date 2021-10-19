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
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == None

    @test
    def findMap03(): Bool =
        LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(2, ENil)) == Some(1)

	@test
	def findMap04(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))) == None

	@test
	def findMap05(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == Some(2)

	@test
	def findMap06(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == Some(3)

	@test
	def findMap07(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == Some(4)

    @test
	def findMap08(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == Some(0)

    @test
	def findMap09(): Bool =
		LazyList.findMap(i -> if (i % 2 == 0) Some(i / 2) else None,
		    LList(lazy LCons(-1, lazy LList(lazy LCons(1, lazy
		    LList(lazy LCons(9, lazy LList(lazy LCons(3, lazy
		    LList(lazy LCons(5, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == Some(5)


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapPure01(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ENil) == ENil

    @test
    def filterMapPure02(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == ENil

    @test
    def filterMapPure03(): Bool =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(2, ENil)) == ECons(1, ENil)

	@test
	def filterMapPure04(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))) == ENil

	@test
	def filterMapPure05(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapPure06(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapPure07(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @test
	def filterMapPure08(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @test
	def filterMapPure09(): Bool =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None,
		    LList(lazy LCons(0, lazy LList(lazy
		    LCons(1, lazy LList(lazy LCons(2, lazy
		    LList(lazy LCons(3, lazy LList(lazy
		    LCons(4, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapImpure01(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ENil) == ENil

    @test
    def filterMapImpure02(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ENil)) == ENil

    @test
    def filterMapImpure03(): Bool & Impure =
        LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(2, ENil)) == ECons(1, ENil)

	@test
	def filterMapImpure04(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ECons(3, ENil))) == ENil

	@test
	def filterMapImpure05(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapImpure06(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapImpure07(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @test
	def filterMapImpure08(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @test
	def filterMapImpure09(): Bool & Impure =
		LazyList.filterMap(i -> if (i % 2 == 0) Some(i / 2) else None as & Impure,
		    LList(lazy LCons(0, lazy LList(lazy
		    LCons(1, lazy LList(lazy LCons(2, lazy
		    LList(lazy LCons(3, lazy LList(lazy
		    LCons(4, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


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
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3) else None as & Impure) == ECons(6, ECons(9, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap filterMap fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.filterMap(x -> { l := "a" :: deref l; Some(x) }) |>
        LazyList.filterMap(x -> { l := "b" :: deref l; Some(x) });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.filterMap(x -> { l := "a" :: deref l; Some(x) } as & Pure) |>
        LazyList.filterMap(x -> { l := "b" :: deref l; Some(x) } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        LazyList.toString(ECons(1, ECons(2, ECons(3, ENil)))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString04(): Bool =
        LazyList.toString(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == "1 :: 2 :: 3 :: ENil"


    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def partition01(): Bool =
        LazyList.partition(i -> i > 3, ENil) == (ENil, ENil)

    @test
    def partition02(): Bool =
        LazyList.partition(i -> i > 3, ECons(1, ENil)) == (ENil, ECons(1, ENil))

    @test
    def partition03(): Bool =
        LazyList.partition(i -> i > 3, ECons(4, ENil)) == (ECons(4, ENil), ENil)

    @test
    def partition04(): Bool =
        LazyList.partition(i -> i > 3, ECons(1, ECons(2, ENil))) == (ENil, ECons(1, ECons(2, ENil)))

    @test
    def partition05(): Bool =
        LazyList.partition(i -> i > 3, ECons(1, ECons(5, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def partition06(): Bool =
        LazyList.partition(i -> i > 3, ECons(5, ECons(1, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def partition07(): Bool =
        LazyList.partition(i -> i > 3, ECons(5, ECons(8, ENil))) == (ECons(5, ECons(8, ENil)), ENil)

    @test
    def partition08(): Bool =
        LazyList.partition(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))))
            == (ECons(4, ECons(6, ECons(11, ECons(16, ECons(7, ECons(7, ENil)))))), ECons(-3, ECons(-5, ECons(1, ECons(2, ECons(1, ENil))))))

    @test
    def partition09(): Bool =
        LazyList.partition(i -> i > 3,
            LList(lazy LCons(4, lazy LList(lazy
                ECons(6, LCons(-3, lazy LList(lazy
                LCons(11, lazy LList(lazy LCons(-5, lazy
                LList(lazy LCons(1, lazy LList(lazy
                LCons(2, lazy LList(lazy LCons(16, lazy
                LList(lazy LCons(7, lazy LList(lazy
                LCons(1, lazy LList(lazy LCons(7, lazy LList(lazy ENil)))))))))))))))))))))))
            == (ECons(4, ECons(6, ECons(11, ECons(16, ECons(7, ECons(7, ENil)))))),
                    ECons(-3, ECons(-5, ECons(1, ECons(2, ECons(1, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def span01(): Bool =
        LazyList.span(i -> i > 3, ENil) == (ENil, ENil)

    @test
    def span02(): Bool =
        LazyList.span(i -> i > 3, ECons(1, ENil)) == (ENil, ECons(1, ENil))

    @test
    def span03(): Bool =
        LazyList.span(i -> i > 3, ECons(4, ENil)) == (ECons(4, ENil), ENil)

    @test
    def span04(): Bool =
        LazyList.span(i -> i > 3, ECons(1, ECons(2, ENil))) == (ENil, ECons(1, ECons(2, ENil)))

    @test
    def span05(): Bool =
        LazyList.span(i -> i > 3, ECons(1, ECons(5, ENil))) == (ENil, ECons(1, ECons(5, ENil)))

    @test
    def span06(): Bool =
        LazyList.span(i -> i > 3, ECons(5, ECons(1, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def span07(): Bool =
        LazyList.span(i -> i > 3, ECons(5, ECons(8, ENil))) == (ECons(5, ECons(8, ENil)), ENil)

    @test
    def span08(): Bool =
        LazyList.span(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == (ECons(4, ECons(6, ENil)), ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))

    @test
    def span09(): Bool =
        LazyList.span(i -> i > 3,
            LList(lazy LCons(4, lazy LList(lazy
                LCons(6, lazy LList(lazy LCons(-3, lazy
                LList(lazy LCons(11, lazy LList(lazy
                LCons(-5, lazy LList(lazy LCons(1, lazy
                LList(lazy LCons(2, lazy LList(lazy
                LCons(16, lazy LList(lazy LCons(7, lazy
                LList(lazy LCons(1, lazy LList(lazy
                LCons(7, lazy LList(lazy ENil))))))))))))))))))))))))
            == (ECons(4, ECons(6, ENil)),
                    ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        List.toLazyList(Nil) |> LazyList.sum == 0

    @test
    def sum02(): Bool =
        List.toLazyList(1 :: Nil) |> LazyList.sum == 1

    @test
    def sum03(): Bool =
        List.toLazyList(1 :: 2 :: 3 :: Nil) |> LazyList.sum == 6

    @test
    def sum04(): Bool =
        List.toLazyList(1 :: 2 :: 3 :: -3 :: Nil) |> LazyList.sum == 3

    @test
    def sum05(): Bool =
        List.toLazyList(-1 :: -2 :: -3 :: -4 :: Nil) |> LazyList.sum == -10

    @test
    def sum06(): Bool =
        List.toLazyList(10 :: -10 :: Nil) |> LazyList.sum == 0

    @test
    def sum07(): Bool =
        List.range(1, 101) |> List.toLazyList |> LazyList.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // order                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def order01(): Bool =
        ENil: LazyList[Unit] <=> ENil: LazyList[Unit] == EqualTo

    @test
    def order02(): Bool =
        ((1 :: Nil) |> List.toLazyList) <=> ENil: LazyList[Int32] == GreaterThan

    @test
    def order03(): Bool =
        ENil: LazyList[Int32] <=> ((1 :: Nil) |> List.toLazyList) == LessThan

    @test
    def order04(): Bool =
        ((1 :: Nil) |> List.toLazyList) <=> ((1 :: Nil) |> List.toLazyList) == EqualTo

    @test
    def order05(): Bool =
        ((2 :: 1 :: Nil) |> List.toLazyList) <=> ((1 :: 1 :: Nil) |> List.toLazyList) == GreaterThan

    @test
    def order06(): Bool =
        ((1 :: 1 :: Nil) |> List.toLazyList) <=> ((2 :: 1 :: Nil) |> List.toLazyList) == LessThan

    @test
    def order07(): Bool =
        ((1 :: Nil) |> List.toLazyList) <=> ((1 :: 1 :: Nil) |> List.toLazyList) == LessThan

}