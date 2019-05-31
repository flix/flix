/*
 * Copyright 2017 Liam Palmer
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

namespace TestSet {

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01(): Bool = Set.size(#{}) `assertEq!` 0

@test
def size02(): Bool = Set.size(#{1}) `assertEq!` 1

@test
def size03(): Bool = Set.size(#{1, 2}) `assertEq!` 2

@test
def size04(): Bool = Set.size(#{1, 2, 3}) `assertEq!` 3

@test
def size05(): Bool = Set.size(#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14}) `assertEq!` 13

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def empty01(): Bool = assert!(Set.eq(Set.empty(), #{}))

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def singleton01(): Bool = assert!(Set.eq(Set.singleton(1), #{1}))

@test
def singleton02(): Bool = assert!(Set.eq(Set.singleton(2), #{2}))

@test
def singleton03(): Bool = assert!(Set.eq(Set.singleton(true), #{true}))

@test
def singleton04(): Bool = assert!(Set.eq(Set.singleton(false), #{false}))

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def insert01(): Bool = assert!(Set.eq(Set.insert(1, #{}), #{1}))

@test
def insert02(): Bool = assert!(Set.eq(Set.insert(1, #{1}), #{1}))

@test
def insert03(): Bool = assert!(Set.eq(Set.insert(1, #{2}), #{1, 2}))

@test
def insert04(): Bool = assert!(Set.eq(Set.insert(1, #{2, 3}), #{1, 2, 3}))

@test
def insert05(): Bool = assert!(Set.eq(Set.insert(1, #{1, 2, 3}), #{1, 2, 3}))

@test
def insert06(): Bool = assert!(Set.eq(Set.insert(1, #{2, 1, 3}), #{2, 1, 3}))

@test
def insert07(): Bool = assert!(Set.eq(Set.insert(1, #{2, 1, 3, 7, 8, 9, 10}), #{2, 1, 3, 7, 8, 9, 10}))

@test
def insert08(): Bool = assert!(Set.eq(Set.insert(11, #{2, 1, 3, 7, 8, 9, 10, 35}), #{11, 2, 1, 3, 7, 8, 9, 10, 35}))

/////////////////////////////////////////////////////////////////////////////
// delete                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def delete01(): Bool = assert!(Set.eq(Set.delete(1, #{}), #{}))

@test
def delete02(): Bool = assert!(Set.eq(Set.delete(1, #{1}), #{}))

@test
def delete03(): Bool = assert!(Set.eq(Set.delete(1, #{2}), #{2}))

@test
def delete04(): Bool = assert!(Set.eq(Set.delete(2, #{2, 3}), #{3}))

@test
def delete05(): Bool = assert!(Set.eq(Set.delete(3, #{2, 3}), #{2}))

@test
def delete06(): Bool = assert!(Set.eq(Set.delete(1, #{2, 3}), #{2, 3}))

@test
def delete07(): Bool = assert!(Set.eq(Set.delete(0, #{1, 2, 3, 4, 5, 6, 7}), #{1, 2, 3, 4, 5, 6, 7}))

@test
def delete08(): Bool = assert!(Set.eq(Set.delete(1, #{1, 2, 3, 4, 5, 6, 7}), #{2, 3, 4, 5, 6, 7}))

@test
def delete09(): Bool = assert!(Set.eq(Set.delete(2, #{1, 2, 3, 4, 5, 6, 7}), #{1, 3, 4, 5, 6, 7}))

@test
def delete10(): Bool = assert!(Set.eq(Set.delete(6, #{1, 2, 3, 4, 5, 6, 7}), #{1, 2, 3, 4, 5, 7}))

@test
def delete11(): Bool = assert!(Set.eq(Set.delete(7, #{1, 2, 3, 4, 5, 6, 7}), #{1, 2, 3, 4, 5, 6}))

@test
def delete12(): Bool = assert!(Set.eq(Set.delete(8, #{1, 2, 3, 4, 5, 6, 7}), #{1, 2, 3, 4, 5, 6, 7}))

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = assert!(Set.isEmpty(#{}))

@test
def isEmpty02(): Bool = assertNot!(Set.isEmpty(#{1}))

@test
def isEmpty03(): Bool = assertNot!(Set.isEmpty(#{1, 2}))

@test
def isEmpty04(): Bool = assertNot!(Set.isEmpty(#{1, 2, 3}))

@test
def isEmpty05(): Bool = assertNot!(Set.isEmpty(#{1, 2, 3, 4, 5, 6, 7, 8}))

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = assertNot!(Set.memberOf(1, #{}))

@test
def memberOf02(): Bool = assertNot!(Set.memberOf(1, #{2}))

@test
def memberOf03(): Bool = assert!(Set.memberOf(1, #{1}))

@test
def memberOf04(): Bool = assert!(Set.memberOf(1, #{1, 2}))

@test
def memberOf05(): Bool = assert!(Set.memberOf(2, #{1, 2}))

@test
def memberOf06(): Bool = assertNot!(Set.memberOf(0, #{1, 2}))

@test
def memberOf07(): Bool = assertNot!(Set.memberOf(3, #{1, 2}))

@test
def memberOf08(): Bool = assertNot!(Set.memberOf(0, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def memberOf09(): Bool = assert!(Set.memberOf(1, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def memberOf10(): Bool = assert!(Set.memberOf(2, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def memberOf11(): Bool = assert!(Set.memberOf(10, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def memberOf12(): Bool = assertNot!(Set.memberOf(12, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Set.find(i -> i > 2, #{}) `assertEq!` None

@test
def find02(): Bool = Set.find(i -> i > 2, #{1}) `assertEq!` None

@test
def find03(): Bool = Set.find(i -> i > 2, #{3}) `assertEq!` Some(3)

@test
def find04(): Bool = Set.find(i -> i > 2, #{2, 1}) `assertEq!` None

@test
def find05(): Bool = Set.find(i -> i > 2, #{-6, 6}) `assertEq!` Some(6)

@test
def find06(): Bool = Set.find(i -> i > 2, #{6, -6}) `assertEq!` Some(6)

@test
def find07(): Bool = Set.find(i -> i > 2, #{7, 6}) `assertEq!` Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def findLeft01(): Bool = Set.findLeft(i -> i > 2, #{}) `assertEq!` None

@test
def findLeft02(): Bool = Set.findLeft(i -> i > 2, #{1}) `assertEq!` None

@test
def findLeft03(): Bool = Set.findLeft(i -> i > 2, #{3}) `assertEq!` Some(3)

@test
def findLeft04(): Bool = Set.findLeft(i -> i > 2, #{2, 1}) `assertEq!` None

@test
def findLeft05(): Bool = Set.findLeft(i -> i > 2, #{-6, 6}) `assertEq!` Some(6)

@test
def findLeft06(): Bool = Set.findLeft(i -> i > 2, #{6, -6}) `assertEq!` Some(6)

@test
def findLeft07(): Bool = Set.findLeft(i -> i > 2, #{7, 6}) `assertEq!` Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def findRight01(): Bool = Set.findRight(i -> i > 2, #{}) `assertEq!` None

@test
def findRight02(): Bool = Set.findRight(i -> i > 2, #{1}) `assertEq!` None

@test
def findRight03(): Bool = Set.findRight(i -> i > 2, #{3}) `assertEq!` Some(3)

@test
def findRight04(): Bool = Set.findRight(i -> i > 2, #{2, 1}) `assertEq!` None

@test
def findRight05(): Bool = Set.findRight(i -> i > 2, #{-6, 6}) `assertEq!` Some(6)

@test
def findRight06(): Bool = Set.findRight(i -> i > 2, #{6, -6}) `assertEq!` Some(6)

@test
def findRight07(): Bool = Set.findRight(i -> i > 2, #{7, 6}) `assertEq!` Some(7)

/////////////////////////////////////////////////////////////////////////////
// isSubsetOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSubsetOf01(): Bool = assert!(Set.isSubsetOf(#{}, #{}))

@test
def isSubsetOf02(): Bool = assert!(Set.isSubsetOf(#{}, #{1}))

@test
def isSubsetOf03(): Bool = assert!(Set.isSubsetOf(#{1}, #{1}))

@test
def isSubsetOf04(): Bool = assertNot!(Set.isSubsetOf(#{1}, #{2}))

@test
def isSubsetOf05(): Bool = assert!(Set.isSubsetOf(#{}, #{1, 2}))

@test
def isSubsetOf06(): Bool = assert!(Set.isSubsetOf(#{1}, #{1, 2}))

@test
def isSubsetOf07(): Bool = assert!(Set.isSubsetOf(#{2}, #{1, 2}))

@test
def isSubsetOf08(): Bool = assertNot!(Set.isSubsetOf(#{3}, #{1, 2}))

@test
def isSubsetOf09(): Bool = assert!(Set.isSubsetOf(#{1, 2}, #{1, 2}))

@test
def isSubsetOf10(): Bool = assert!(Set.isSubsetOf(#{2, 1}, #{1, 2}))

@test
def isSubsetOf11(): Bool = assertNot!(Set.isSubsetOf(#{3, 1}, #{1, 2}))

@test
def isSubsetOf12(): Bool = assertNot!(Set.isSubsetOf(#{1, 2, 3}, #{1, 2}))

@test
def isSubsetOf13(): Bool = assert!(Set.isSubsetOf(#{10}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isSubsetOf14(): Bool = assert!(Set.isSubsetOf(#{9, 1}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isSubsetOf15(): Bool = assert!(Set.isSubsetOf(#{6, 5, 8}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isSubsetOf16(): Bool = assertNot!(Set.isSubsetOf(#{10, 2, 3, 6, -1, 4}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

/////////////////////////////////////////////////////////////////////////////
// isProperSubsetOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def isProperSubsetOf01(): Bool = assertNot!(Set.isProperSubsetOf(#{}, #{}))

@test
def isProperSubsetOf02(): Bool = assert!(Set.isProperSubsetOf(#{}, #{1}))

@test
def isProperSubsetOf03(): Bool = assertNot!(Set.isProperSubsetOf(#{1}, #{1}))

@test
def isProperSubsetOf04(): Bool = assertNot!(Set.isProperSubsetOf(#{1}, #{2}))

@test
def isProperSubsetOf05(): Bool = assert!(Set.isProperSubsetOf(#{}, #{1, 2}))

@test
def isProperSubsetOf06(): Bool = assert!(Set.isProperSubsetOf(#{1}, #{1, 2}))

@test
def isProperSubsetOf07(): Bool = assert!(Set.isProperSubsetOf(#{2}, #{1, 2}))

@test
def isProperSubsetOf08(): Bool = assertNot!(Set.isProperSubsetOf(#{3}, #{1, 2}))

@test
def isProperSubsetOf09(): Bool = assertNot!(Set.isProperSubsetOf(#{1, 2}, #{1, 2}))

@test
def isProperSubsetOf10(): Bool = assertNot!(Set.isProperSubsetOf(#{2, 1}, #{1, 2}))

@test
def isProperSubsetOf11(): Bool = assertNot!(Set.isProperSubsetOf(#{3, 1}, #{1, 2}))

@test
def isProperSubsetOf12(): Bool = assertNot!(Set.isProperSubsetOf(#{1, 2, 3}, #{1, 2}))

@test
def isProperSubsetOf13(): Bool = assert!(Set.isProperSubsetOf(#{10}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isProperSubsetOf14(): Bool = assert!(Set.isProperSubsetOf(#{9, 1}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isProperSubsetOf15(): Bool = assert!(Set.isProperSubsetOf(#{6, 5, 8}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isProperSubsetOf16(): Bool = assertNot!(Set.isProperSubsetOf(#{10, 2, 3, 6, -1, 4}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isProperSubsetOf17(): Bool = assertNot!(Set.isProperSubsetOf(#{10, 2, 3, 6, 1, 4, 5, 9, 8, 7}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def fold01(): Bool = Set.fold((i, e) -> (i - e)*(e % 2 + 1), 100, #{}) `assertEq!` 100

@test
def fold02(): Bool = Set.fold((i, e) -> (i - e)*(e % 2 + 1), 100, #{1}) `assertEq!` 198

@test
def fold03(): Bool = Set.fold((i, e) -> (i - e)*(e % 2 + 1), 100, #{2, 1}) `assertEq!` 196

@test
def fold04(): Bool = Set.fold((i, e) -> (i - e)*(e % 2 + 1), 100, #{3, 2, 1}) `assertEq!` 386

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = Set.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, #{}) `assertEq!` 100

@test
def foldLeft02(): Bool = Set.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, #{1}) `assertEq!` 198

@test
def foldLeft03(): Bool = Set.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, #{2, 1}) `assertEq!` 196

@test
def foldLeft04(): Bool = Set.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, #{3, 2, 1}) `assertEq!` 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = Set.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, #{}) `assertEq!` 100

@test
def foldRight02(): Bool = Set.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, #{1}) `assertEq!` 198

@test
def foldRight03(): Bool = Set.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, #{2, 1}) `assertEq!` 194

@test
def foldRight04(): Bool = Set.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, #{3, 2, 1}) `assertEq!` 382

/////////////////////////////////////////////////////////////////////////////
// reduce                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def reduce01(): Bool = Set.reduce((a, b) -> a-b, #{}: Set[Int]) `assertEq!` None

@test
def reduce02(): Bool = Set.reduce((a, b) -> a-b, #{1}) `assertEq!` Some(1)

@test
def reduce03(): Bool = Set.reduce((a, b) -> a-b, #{2, 1}) `assertEq!` Some(-1)

@test
def reduce04(): Bool = Set.reduce((a, b) -> a-b, #{3, 2, 1}) `assertEq!` Some(-4)

@test
def reduce05(): Bool = Set.reduce((a, b) -> a-b, #{4, 3, 2, 1}) `assertEq!` Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeft01(): Bool = Set.reduceLeft((a, b) -> a-b, #{}: Set[Int]) `assertEq!` None

@test
def reduceLeft02(): Bool = Set.reduceLeft((a, b) -> a-b, #{1}) `assertEq!` Some(1)

@test
def reduceLeft03(): Bool = Set.reduceLeft((a, b) -> a-b, #{2, 1}) `assertEq!` Some(-1)

@test
def reduceLeft04(): Bool = Set.reduceLeft((a, b) -> a-b, #{3, 2, 1}) `assertEq!` Some(-4)

@test
def reduceLeft05(): Bool = Set.reduceLeft((a, b) -> a-b, #{4, 3, 2, 1}) `assertEq!` Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRight01(): Bool = Set.reduceRight((a, b) -> a-b, #{}: Set[Int]) `assertEq!` None

@test
def reduceRight02(): Bool = Set.reduceRight((a, b) -> a-b, #{1}) `assertEq!` Some(1)

@test
def reduceRight03(): Bool = Set.reduceRight((a, b) -> a-b, #{2, 1}) `assertEq!` Some(-1)

@test
def reduceRight04(): Bool = Set.reduceRight((a, b) -> a-b, #{3, 2, 1}) `assertEq!` Some(2)

@test
def reduceRight05(): Bool = Set.reduceRight((a, b) -> a-b, #{4, 3, 2, 1}) `assertEq!` Some(-2)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = Set.count(i -> i > 3, #{}) `assertEq!` 0

@test
def count02(): Bool = Set.count(i -> i > 3, #{1}) `assertEq!` 0

@test
def count03(): Bool = Set.count(i -> i > 3, #{4}) `assertEq!` 1

@test
def count04(): Bool = Set.count(i -> i > 3, #{2, 1}) `assertEq!` 0

@test
def count05(): Bool = Set.count(i -> i > 3, #{8, 1}) `assertEq!` 1

@test
def count06(): Bool = Set.count(i -> i > 3, #{1, 8}) `assertEq!` 1

@test
def count07(): Bool = Set.count(i -> i > 3, #{7, 6}) `assertEq!` 2

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatten01(): Bool = assert!(Set.eq(Set.flatten(#{}), #{}))

@test
def flatten02(): Bool = assert!(Set.eq(Set.flatten(#{#{}}), #{}))

@test
def flatten03(): Bool = assert!(Set.eq(Set.flatten(#{#{1}}), #{1}))

@test
def flatten04(): Bool = assert!(Set.eq(Set.flatten(#{#{1, 2}}), #{1, 2}))

@test
def flatten05(): Bool = assert!(Set.eq(Set.flatten(#{#{}, #{}}), #{}))

@test
def flatten06(): Bool = assert!(Set.eq(Set.flatten(#{#{1}, #{}}), #{1}))

@test
def flatten07(): Bool = assert!(Set.eq(Set.flatten(#{#{}, #{1}}), #{1}))

@test
def flatten08(): Bool = assert!(Set.eq(Set.flatten(#{#{1}, #{2}}), #{1, 2}))

@test
def flatten09(): Bool = assert!(Set.eq(Set.flatten(#{#{1}, #{1}}), #{1}))

@test
def flatten10(): Bool = assert!(Set.eq(Set.flatten(#{#{1, 2}, #{3, 4, 5}}), #{1, 2, 3, 4, 5}))

@test
def flatten11(): Bool = assert!(Set.eq(Set.flatten(#{#{1, 2}, #{3, 4, 3}}), #{1, 2, 3, 4}))

@test
def flatten12(): Bool = assert!(Set.eq(Set.flatten(#{#{1, 2}, #{3, 4, 1}}), #{1, 2, 3, 4}))

@test
def flatten13(): Bool = assert!(Set.eq(Set.flatten(#{#{1}, #{2, 3}, #{4}}), #{1, 2, 3, 4}))

@test
def flatten14(): Bool = assert!(Set.eq(Set.flatten(#{#{1}, #{2, 3}, #{1}}), #{1, 2, 3}))

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = assertNot!(Set.exists(x -> x % 8 == 7, #{}))

@test
def exists02(): Bool = assertNot!(Set.exists(x -> x % 8 == 7, #{5}))

@test
def exists03(): Bool = assert!(Set.exists(x -> x % 8 == 7, #{7}))

@test
def exists04(): Bool = assert!(Set.exists(x -> x % 8 == 7, #{15}))

@test
def exists05(): Bool = assertNot!(Set.exists(x -> x % 8 == 7, #{1, 44}))

@test
def exists06(): Bool = assert!(Set.exists(x -> x % 8 == 7, #{11, 71}))

@test
def exists07(): Bool = assert!(Set.exists(x -> x % 8 == 7, #{71, 12}))

@test
def exists08(): Bool = assert!(Set.exists(x -> x % 8 == 7, #{71, 79}))

@test
def exists09(): Bool = assertNot!(Set.exists(x -> x % 8 == 7, #{11, -1, -14, -2, 84, 113}))

@test
def exists10(): Bool = assert!(Set.exists(x -> x % 8 == 7, #{11, -1, 31, -14, -2, 84, 111}))

@test
def exists11(): Bool = assert!(Set.exists(x -> x % 8 == 7, #{11, -1, -14, -2, 84, 111, 38}))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = assert!(Set.forall(x -> x % 8 == 7, #{}))

@test
def forall02(): Bool = assertNot!(Set.forall(x -> x % 8 == 7, #{5}))

@test
def forall03(): Bool = assert!(Set.forall(x -> x % 8 == 7, #{7}))

@test
def forall04(): Bool = assert!(Set.forall(x -> x % 8 == 7, #{15}))

@test
def forall05(): Bool = assertNot!(Set.forall(x -> x % 8 == 7, #{1, 44}))

@test
def forall06(): Bool = assertNot!(Set.forall(x -> x % 8 == 7, #{11, 71}))

@test
def forall07(): Bool = assertNot!(Set.forall(x -> x % 8 == 7, #{71, 12}))

@test
def forall08(): Bool = assert!(Set.forall(x -> x % 8 == 7, #{71, 79}))

@test
def forall09(): Bool = assertNot!(Set.forall(x -> x % 8 == 7, #{7, 15, 23, 84, 111}))

@test
def forall10(): Bool = assertNot!(Set.forall(x -> x % 8 == 7, #{7, 15, 23, 111, 3}))

@test
def forall11(): Bool = assert!(Set.forall(x -> x % 8 == 7, #{7, 15, 23, 119, 111}))

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def union01(): Bool = assert!(Set.eq(Set.union(#{}, #{}), #{}))

@test
def union02(): Bool = assert!(Set.eq(Set.union(#{1}, #{}), #{1}))

@test
def union03(): Bool = assert!(Set.eq(Set.union(#{}, #{2}), #{2}))

@test
def union04(): Bool = assert!(Set.eq(Set.union(#{1}, #{1}), #{1}))

@test
def union05(): Bool = assert!(Set.eq(Set.union(#{1}, #{-1}), #{1, -1}))

@test
def union06(): Bool = assert!(Set.eq(Set.union(#{}, #{-1, 9}), #{-1, 9}))

@test
def union07(): Bool = assert!(Set.eq(Set.union(#{9}, #{-1, 9}), #{-1, 9}))

@test
def union08(): Bool = assert!(Set.eq(Set.union(#{4}, #{-1, 9}), #{4, -1, 9}))

@test
def union09(): Bool = assert!(Set.eq(Set.union(#{9, -1}, #{-1, 9}), #{-1, 9}))

@test
def union10(): Bool = assert!(Set.eq(Set.union(#{9, 5}, #{-1, 9}), #{5, -1, 9}))

@test
def union11(): Bool = assert!(Set.eq(Set.union(#{6, 5}, #{-1, 9}), #{6, 5, -1, 9}))

@test
def union12(): Bool = assert!(Set.eq(Set.union(#{6, -99}, #{6, 5, -1, 9, 43, 7, 8, -99}), #{6, 5, -1, 9, 43, 7, 8, -99}))

@test
def union13(): Bool = assert!(Set.eq(Set.union(#{6, -99, -1, 5, 22}, #{6, 5, -1, 9, 43, 7, 8, -99}), #{22, 6, 5, -1, 9, 43, 7, 8, -99}))

@test
def union14(): Bool = assert!(Set.eq(Set.union(#{-2, -3, -4}, #{6, 5, -1, 9, 43, 7, 8, -99}), #{-2, -3, -4, 6, 5, -1, 9, 43, 7, 8, -99}))

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def intersection01(): Bool = assert!(Set.eq(Set.intersection(#{}, #{}), #{}))

@test
def intersection02(): Bool = assert!(Set.eq(Set.intersection(#{1}, #{}), #{}))

@test
def intersection03(): Bool = assert!(Set.eq(Set.intersection(#{}, #{2}), #{}))

@test
def intersection04(): Bool = assert!(Set.eq(Set.intersection(#{1}, #{2}), #{}))

@test
def intersection05(): Bool = assert!(Set.eq(Set.intersection(#{1}, #{1}), #{1}))

@test
def intersection06(): Bool = assert!(Set.eq(Set.intersection(#{}, #{1, 2}), #{}))

@test
def intersection07(): Bool = assert!(Set.eq(Set.intersection(#{1, 2}, #{}), #{}))

@test
def intersection08(): Bool = assert!(Set.eq(Set.intersection(#{2}, #{1, 2}), #{2}))

@test
def intersection09(): Bool = assert!(Set.eq(Set.intersection(#{1}, #{1, 2}), #{1}))

@test
def intersection10(): Bool = assert!(Set.eq(Set.intersection(#{2, 1}, #{1, 2}), #{2, 1}))

@test
def intersection11(): Bool = assert!(Set.eq(Set.intersection(#{1, 2}, #{1, 2}), #{1, 2}))

@test
def intersection12(): Bool = assert!(Set.eq(Set.intersection(#{3, 2}, #{1, 2}), #{2}))

@test
def intersection13(): Bool = assert!(Set.eq(Set.intersection(#{3, 55}, #{1, 2}), #{}))

@test
def intersection14(): Bool = assert!(Set.eq(Set.intersection(#{3, 55, 11, 87, 22, 34, -87, 23}, #{1, 2, 84, -87, 87, 3, 44}), #{3, 87, -87}))

@test
def intersection15(): Bool = assert!(Set.eq(Set.intersection(#{3, 55, 11, 87, 22, 34, -87, 23}, #{23, 1, 2, 84, 87, 3}), #{3, 87, 23}))

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def difference01(): Bool = assert!(Set.eq(Set.difference(#{}, #{}), #{}))

@test
def difference02(): Bool = assert!(Set.eq(Set.difference(#{}, #{2}), #{}))

@test
def difference03(): Bool = assert!(Set.eq(Set.difference(#{1}, #{}), #{1}))

@test
def difference04(): Bool = assert!(Set.eq(Set.difference(#{1}, #{2}), #{1}))

@test
def difference05(): Bool = assert!(Set.eq(Set.difference(#{1}, #{1}), #{}))

@test
def difference06(): Bool = assert!(Set.eq(Set.difference(#{1, 2}, #{}), #{1, 2}))

@test
def difference07(): Bool = assert!(Set.eq(Set.difference(#{1, 2}, #{1}), #{2}))

@test
def difference08(): Bool = assert!(Set.eq(Set.difference(#{1, 2}, #{2}), #{1}))

@test
def difference09(): Bool = assert!(Set.eq(Set.difference(#{1, 2}, #{8, 2, 4}), #{1}))

@test
def difference10(): Bool = assert!(Set.eq(Set.difference(#{1, 2}, #{3, 1, 2, 4}), #{}))

@test
def difference11(): Bool = assert!(Set.eq(Set.difference(#{1, 2}, #{3, 11, 21, 4}), #{1, 2}))

@test
def difference12(): Bool = assert!(Set.eq(Set.difference(#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, #{-1, 92, 4, 5, 1, 2, 86}), #{87, 6, 111}))

@test
def difference13(): Bool = assert!(Set.eq(Set.difference(#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, #{-1, 92, 4, 5, 1, 2, 86, 99, 6}), #{87, 111}))

/////////////////////////////////////////////////////////////////////////////
// subsets                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def subsets01(): Bool = assert!(Set.eq(Set.subsets(#{}), #{#{}}))

@test
def subsets02(): Bool = assert!(Set.eq(Set.subsets(#{1}), #{#{1}, #{}}))

@test
def subsets03(): Bool = assert!(Set.eq(Set.subsets(#{1, 2}), #{#{1, 2}, #{1}, #{2}, #{}}))

@test
def subsets04(): Bool = assert!(Set.eq(Set.subsets(#{1, 2, 3}), #{#{1, 2, 3}, #{1, 2}, #{1, 3}, #{1}, #{2, 3}, #{2}, #{3}, #{}}))

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{}), #{}))

@test
def filter02(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{1}), #{}))

@test
def filter03(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{2}), #{2}))

@test
def filter04(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{1, 3}), #{}))

@test
def filter05(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{8, 3}), #{8}))

@test
def filter06(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{-1, 32}), #{32}))

@test
def filter07(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{12, 34}), #{12, 34}))

@test
def filter08(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{-33, -1, 12, 1, 34, 88, 7, 77, 31}), #{12, 34, 88}))

@test
def filter09(): Bool = assert!(Set.eq(Set.filter(x -> x % 2 == 0, #{-33, -1, 12, 1, 34, 88, 7, 77, 31, 7, -92, 841}), #{12, 34, 88, -92}))

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{}), #{}))

@test
def map02(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{1}), #{false}))

@test
def map03(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{2}), #{true}))

@test
def map04(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{1, -1}), #{false}))

@test
def map05(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{1, -12}), #{false, true}))

@test
def map06(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{16, -1}), #{true, false}))

@test
def map07(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{12, -12}), #{true}))

@test
def map08(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{12, -12}), #{true}))

@test
def map09(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{12, -12, 1, 14}), #{false, true}))

@test
def map10(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{12, -12, 1, 14, 7, 88, -91}), #{true, false}))

@test
def map11(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{12, -12, 122, 14}), #{true}))

@test
def map12(): Bool = assert!(Set.eq(Set.map(x -> x % 2 == 0, #{123, -123, 1223, 141}), #{false}))

@test
def map13(): Bool = assert!(Set.eq(Set.map(x -> x % 9, #{11, 5, 16, 4}), #{2, 5, 7, 4}))

@test
def map14(): Bool = assert!(Set.eq(Set.map(x -> x % 9, #{0, 5, 1, -9, -8}), #{5, 1, 0, -8}))

@test
def map15(): Bool = assert!(Set.eq(Set.map(x -> x % 9, #{0, 5, 1, 10, 7, 19, 28, 2}), #{0, 5, 7, 1, 2}))

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01(): Bool = assert!(Set.eq(Set.flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{}), #{}))

@test
def flatMap02(): Bool = assert!(Set.eq(Set.flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{2}), #{}))

@test
def flatMap03(): Bool = assert!(Set.eq(Set.flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{1}), #{1}))

@test
def flatMap04(): Bool = assert!(Set.eq(Set.flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{2, 4}), #{}))

@test
def flatMap05(): Bool = assert!(Set.eq(Set.flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{2, 13}), #{13}))

@test
def flatMap06(): Bool = assert!(Set.eq(Set.flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{15, -8}), #{15}))

@test
def flatMap07(): Bool = assert!(Set.eq(Set.flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{1, 11}), #{1, 11}))

@test
def flatMap08(): Bool = assert!(Set.eq(Set.flatMap(x -> #{x, 2*x}, #{1, 4, 8, 2}), #{1, 8, 16, 2, 4}))

@test
def flatMap09(): Bool = assert!(Set.eq(Set.flatMap(x -> #{x, 3*x}, #{1, 8, 3, 2, 9, -5, -1}), #{1, 8, 24, 3, 2, 6, 9, 27, -5, -15, -1, -3}))

@test
def flatMap10(): Bool = assert!(Set.eq(Set.flatMap(x -> #{x, 2*x, 3*x}, #{1, 4, 3, 2}), #{1, 8, 12, 3, 9, 2, 4, 6}))

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool = assert!(Set.eq(Set.replace(3, 4, #{}), #{}))

@test
def replace02(): Bool = assert!(Set.eq(Set.replace(3, 4, #{1}), #{1}))

@test
def replace03(): Bool = assert!(Set.eq(Set.replace(3, 4, #{3}), #{4}))

@test
def replace04(): Bool = assert!(Set.eq(Set.replace(3, 4, #{4}), #{4}))

@test
def replace05(): Bool = assert!(Set.eq(Set.replace(3, 4, #{1, 2}), #{1, 2}))

@test
def replace06(): Bool = assert!(Set.eq(Set.replace(3, 4, #{1, 3}), #{1, 4}))

@test
def replace07(): Bool = assert!(Set.eq(Set.replace(3, 4, #{3, 2}), #{4, 2}))

@test
def replace08(): Bool = assert!(Set.eq(Set.replace(3, 4, #{3, 4}), #{4}))

/////////////////////////////////////////////////////////////////////////////
// partition                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def partition01(): Bool = Set.partition(x -> x % 2 == 0, #{}) `assertEq!` (#{}, #{})

@test
def partition02(): Bool = Set.partition(x -> x % 2 == 0, #{1}) `assertEq!` (#{}, #{1})

@test
def partition03(): Bool = Set.partition(x -> x % 2 == 0, #{2}) `assertEq!` (#{2}, #{})

@test
def partition04(): Bool = Set.partition(x -> x % 2 == 0, #{1, 3}) `assertEq!` (#{}, #{1, 3})

@test
def partition05(): Bool = Set.partition(x -> x % 2 == 0, #{1, 2}) `assertEq!` (#{2}, #{1})

@test
def partition06(): Bool = Set.partition(x -> x % 2 == 0, #{2, 1}) `assertEq!` (#{2}, #{1})

@test
def partition07(): Bool = Set.partition(x -> x % 2 == 0, #{2, -4}) `assertEq!` (#{2, -4}, #{})

@test
def partition08(): Bool = Set.partition(x -> x % 2 == 0, #{2, -11, 89, -4, 11, -6, 84}) `assertEq!` (#{2, -4, -6, 84}, #{-11, 89, 11})

@test
def partition09(): Bool = Set.partition(x -> x % 2 == 0, #{84, -6, 11, -4, 89, -11, 2}) `assertEq!` (#{84, -6, -4, 2}, #{11, 89, -11})

@test
def partition10(): Bool = Set.partition(x -> x % 2 == 0, #{1, 2, 3, 4, 5, 6, 7, 8}) `assertEq!` (#{2, 4, 6, 8}, #{1, 3, 5, 7})

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = Set.toList(#{}) `assertEq!` Nil

@test
def toList02(): Bool = Set.toList(#{1}) `assertEq!` 1 :: Nil

@test
def toList03(): Bool = Set.toList(#{1, 2}) `assertEq!` 2 :: 1 :: Nil

@test
def toList04(): Bool = Set.toList(#{1, 2, 3}) `assertEq!` 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toMap01(): Bool = assert!(Map.eq(Set.toMap(#{}), @{}))

@test
def toMap02(): Bool = assert!(Map.eq(Set.toMap(#{(1, true)}), @{1 -> true}))

@test
def toMap03(): Bool = assert!(Map.eq(Set.toMap(#{(1, true), (2, false)}), @{1 -> true, 2 -> false}))

@test
def toMap04(): Bool = assert!(Map.eq(Set.toMap(#{(1, true), (1, false)}), @{1 -> false}))

@test
def toMap05(): Bool = assert!(Map.eq(Set.toMap(#{(1, true), (2, false), (3, true)}), @{1 -> true, 2 -> false, 3 -> true}))

/////////////////////////////////////////////////////////////////////////////
// eq                                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def eq01(): Bool = assert!(Set.eq(#{}, #{}))

@test
def eq02(): Bool = assertNot!(Set.eq(#{1}, #{}))

@test
def eq03(): Bool = assertNot!(Set.eq(#{}, #{1}))

@test
def eq04(): Bool = assertNot!(Set.eq(#{1, 2}, #{}))

@test
def eq05(): Bool = assertNot!(Set.eq(#{}, #{1, 2}))

@test
def eq06(): Bool = assertNot!(Set.eq(#{1}, #{2}))

@test
def eq07(): Bool = assert!(Set.eq(#{1}, #{1}))

@test
def eq08(): Bool = assertNot!(Set.eq(#{1, 2, 3}, #{}))

@test
def eq09(): Bool = assertNot!(Set.eq(#{1, 2}, #{1}))

@test
def eq10(): Bool = assertNot!(Set.eq(#{1}, #{1, 2}))

@test
def eq11(): Bool = assertNot!(Set.eq(#{}, #{1, 2, 3}))

@test
def eq12(): Bool = assertNot!(Set.eq(#{1, 2}, #{1, 3}))

@test
def eq13(): Bool = assert!(Set.eq(#{1, 2}, #{2, 1}))

@test
def eq14(): Bool = assert!(Set.eq(#{1, 2}, #{1, 2}))

@test
def eq15(): Bool = assertNot!(Set.eq(#{1, 2, 3}, #{1, 2, 4}))

@test
def eq16(): Bool = assert!(Set.eq(#{1, 2, 3}, #{1, 2, 3}))

@test
def eq17(): Bool = assert!(Set.eq(#{1, 2, 3}, #{2, 3, 1}))

@test
def eq18(): Bool = assert!(Set.eq(#{1, 2, 3}, #{3, 1, 2}))

@test
def eq19(): Bool = assert!(Set.eq(#{1, 2, 3}, #{2, 1, 3}))

}
