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

}