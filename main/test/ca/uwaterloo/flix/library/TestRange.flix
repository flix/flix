/*
 * Copyright 2026 Flix Authors
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

mod TestRange {

    use Range.Range
    use Assert.{assertEq, assertTrue, assertFalse}

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = assertTrue(Range.isEmpty(Range(0, 0)))

    @Test
    def isEmpty02(): Unit \ Assert = assertTrue(Range.isEmpty(Range(5, 5)))

    @Test
    def isEmpty03(): Unit \ Assert = assertFalse(Range.isEmpty(Range(0, 3)))

    @Test
    def isEmpty04(): Unit \ Assert = assertFalse(Range.isEmpty(Range(-3, 0)))

    @Test
    def isEmpty05(): Unit \ Assert = assertTrue(Range.isEmpty(Range(3, 0)))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = assertTrue(Range.memberOf(0, Range(0, 3)))

    @Test
    def memberOf02(): Unit \ Assert = assertTrue(Range.memberOf(2, Range(0, 3)))

    @Test
    def memberOf03(): Unit \ Assert = assertFalse(Range.memberOf(3, Range(0, 3)))

    @Test
    def memberOf04(): Unit \ Assert = assertFalse(Range.memberOf(-1, Range(0, 3)))

    @Test
    def memberOf05(): Unit \ Assert = assertFalse(Range.memberOf(0, Range(0, 0)))

    /////////////////////////////////////////////////////////////////////////////
    // overlaps                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def overlaps01(): Unit \ Assert = assertTrue(Range.overlaps(Range(0, 3), Range(2, 5)))

    @Test
    def overlaps02(): Unit \ Assert = assertTrue(Range.overlaps(Range(2, 5), Range(0, 3)))

    @Test
    def overlaps03(): Unit \ Assert = assertFalse(Range.overlaps(Range(0, 3), Range(3, 5)))

    @Test
    def overlaps04(): Unit \ Assert = assertFalse(Range.overlaps(Range(0, 3), Range(5, 8)))

    @Test
    def overlaps05(): Unit \ Assert = assertTrue(Range.overlaps(Range(0, 10), Range(3, 5)))

    @Test
    def overlaps06(): Unit \ Assert = assertFalse(Range.overlaps(Range(0, 3), Range(1, 1)))

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersection01(): Unit \ Assert = assertEq(expected = Range(3, 5), Range.intersection(Range(0, 5), Range(3, 8)))

    @Test
    def intersection02(): Unit \ Assert = assertEq(expected = Range(2, 5), Range.intersection(Range(0, 10), Range(2, 5)))

    @Test
    def intersection03(): Unit \ Assert = assertTrue(Range.isEmpty(Range.intersection(Range(0, 3), Range(5, 8))))

    @Test
    def intersection04(): Unit \ Assert = assertEq(expected = Range(0, 5), Range.intersection(Range(0, 5), Range(0, 5)))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let sum = Ref.fresh(rc, 0);
        Range.forEach(x -> Ref.put(Ref.get(sum) + x, sum), Range(0, 5));
        assertEq(expected = 10, Ref.get(sum))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let sum = Ref.fresh(rc, 0);
        Range.forEach(x -> Ref.put(Ref.get(sum) + x, sum), Range(0, 0));
        assertEq(expected = 0, Ref.get(sum))
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = 0 :: 1 :: 2 :: Nil, Range.iterator(rc, Range(0, 3)) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = Nil, Range.iterator(rc, Range(0, 0)) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = assertEq(expected = 0 :: 1 :: 2 :: Nil, Range.toList(Range(0, 3)))

    @Test
    def toList02(): Unit \ Assert = assertEq(expected = 2 :: 3 :: 4 :: Nil, Range.toList(Range(2, 5)))

    @Test
    def toList03(): Unit \ Assert = assertEq(expected = Nil, Range.toList(Range(0, 0)))

    @Test
    def toList04(): Unit \ Assert = assertEq(expected = Nil, Range.toList(Range(3, 0)))

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = assertEq(expected = Set#{0, 1, 2}, Range.toSet(Range(0, 3)))

    @Test
    def toSet02(): Unit \ Assert = assertEq(expected = Set#{}, Range.toSet(Range(0, 0)))

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = assertEq(expected = Vector#{0, 1, 2}, Range.toVector(Range(0, 3)))

    @Test
    def toVector02(): Unit \ Assert = assertEq(expected = Vector#{}, Range.toVector(Range(0, 0)))

}
