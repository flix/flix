/*
 * Copyright 2024 Lionel Mendes
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

mod TestData.Range {
    use Data.Range.{all, atLeast, atMost, open, closed, openClosed, closedOpen, lessThan, greaterThan}
    use Data.Range.Enumerable.{range, rangeByStep, zeroTo, toZero, rangeReversed, rangeReversedByStep}

    def nextInt32ByMultiStep(e: Unit -> Option[Int32]): Option[Int32] = {
        match e() {
            case Some(v) if v >= 15                 => Some(v+5)
            case Some(v) if v >= 10                 => Some(v+2)
            case Some(v) if v >= 0                  => Some(v+3)
            case Some(v)                            => Some(v+1)
            case None                               => None
        }
    }

    // Copy from of nextByStep function in Data.Range module (to avoid public function in module)
    // For IteratorBuilder testing purpose
    def nextByStep(step: a, e: Unit -> Option[a]): Option[a] with Add[a] =
         match e() {
            case Some(v)    => Some(v+step)
            case None       => None
        }

    // Copy from of prevByStep function in Data.Range module (to avoid public function in module)
    // For IteratorBuilder testing purpose
    def prevByStep(step: a, e: Unit -> Option[a]): Option[a] with Sub[a] =
         match e() {
            case Some(v)    => Some(v-step)
            case None       => None
        }

    @Test
    def functorInstanceBound(): Bool =
        let expected = Bound.Bound({ boundValue = 1 , boundType = BoundType.Inclusive});
        let b = Bound.Bound({ boundValue = 0 , boundType = BoundType.Inclusive});
        let actual = Data.Range.mapBound(x -> x + 1, b);
        Assert.eq(expected, actual)

    @Test
    def functorInstanceRange01(): Bool =
        let expected = closed(1, 2);
        let r = closed(0, 1);
        let actual = Data.Range.map(x -> x + 1, r);
        Assert.eq(expected, actual)

    @Test
    def functorInstanceRange02(): Bool =
        let expected = Range.Empty;
        let r = open(0, 0);
        let actual = Data.Range.map(x -> x + 1, r);
        Assert.eq(expected, actual)

    @Test
    def iteratorBuilder01(): Bool = region rc {
        let expected = List#{-2,-1,0,1,2,3,4,5,6,7,8,9};
        let r = closed(-2, 9);
        let it = IteratorFactory.IteratorFactory({range=r, producer = nextByStep(1)
                                                , isToBoundReached = (x,y) -> if (x <= y ) true else false});
        let v = MutList.new(rc);
        foreach (x <- it) {  MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def iteratorBuilder02(): Bool = region rc {
        let expected = List#{9,8,7,6,5,4,3,2,1,0,-1};
        let r = closed(-2, 9);
        let it = IteratorFactory.IteratorFactory({range = Data.Range.reverseRange(r), producer = prevByStep(1)
                                                , isToBoundReached = (x,y) -> if (x > y ) true else false});
        let v = MutList.new(rc);
        foreach (x <- it) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def iteratorBuilder03(): Bool = region rc {
        let expected = List#{-2,-1,0,3,6,9,12,14,16,21,26};
        let r = closed(-2, 27);
        let it = IteratorFactory.IteratorFactory({range=r, producer = TestData.Range.nextInt32ByMultiStep
                                                , isToBoundReached = (x,y) -> if (x <= y ) true else false});
        let v = MutList.new(rc);
        foreach (x <- it) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def iteratorBuilder04(): Bool = region rc {
        let expected = List#{};
        let r = closed(5, 2);
        let it = IteratorFactory.IteratorFactory({range = Data.Range.reverseRange(r), producer = prevByStep(1)
                                                , isToBoundReached = (x,y) -> if (x > y ) true else false});
        let v = MutList.new(rc);
        foreach (x <- it) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def zeroTo01(): Bool = region rc {
        let expected = List#{0,1,2,3,4,5};
        let v = MutList.new(rc);
        foreach (x <- zeroTo(5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def toZero01(): Bool = region rc {
        let expected = List#{5,4,3,2,1,0};
        let v = MutList.new(rc);
        foreach (x <- toZero(5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range01(): Bool = region rc {
        let expected = List#{5,6,7,8,9};
        let v = MutList.new(rc);
        foreach (x <- range(closed(5, 9))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range02(): Bool = region rc {
        let expected = List#{5,6,7,8};
        let v = MutList.new(rc);
        foreach (x <- range(closedOpen(5, 9))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range03(): Bool = region rc {
        let expected = List#{6,7,8,9};
        let v = MutList.new(rc);
        foreach (x <- range(openClosed(5, 9))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range04(): Bool = region rc {
        let expected = List#{6,7,8};
        let v = MutList.new(rc);
        foreach (x <- range(open(5, 9))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range05(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- range(open(5, 5))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range06(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- range(openClosed(5, 5))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range07(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- range(closedOpen(5, 5))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeByStep01(): Bool = region rc {
        let expected = List#{0,5,10,15,20};
        let v = MutList.new(rc);
        foreach (x <- rangeByStep(closed(0, 22), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeByStep02(): Bool = region rc {
        let expected = List#{0,5,10,15,20};
        let v = MutList.new(rc);
        foreach (x <- rangeByStep(closedOpen(0, 22), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeByStep03(): Bool = region rc {
        let expected = List#{0,5,10,15};
        let v = MutList.new(rc);
        foreach (x <- rangeByStep(closedOpen(0, 20), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeByStep04(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- rangeByStep(open(20, 20), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeByStep05(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- rangeByStep(all(), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeByStep06(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- rangeByStep(Range.Empty, 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversed01(): Bool = region rc {
        let expected = List#{9,8,7,6,5};
        let v = MutList.new(rc);
        foreach (x <- rangeReversed(closed(9, 5))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversed02(): Bool = region rc {
        let expected = List#{9,8,7,6};
        let v = MutList.new(rc);
        foreach (x <- rangeReversed(closedOpen(9, 5))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversed03(): Bool = region rc {
        let expected = List#{8,7,6,5};
        let v = MutList.new(rc);
        foreach (x <- rangeReversed(openClosed(9, 5))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversed04(): Bool = region rc {
        let expected = List#{8,7,6};
        let v = MutList.new(rc);
        foreach (x <- rangeReversed(open(9, 5))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversed05(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- rangeReversed(open(5, 5))) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversed06(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- rangeReversed(all(): Range[Int32])) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversed07(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- rangeReversed(Range.Empty: Range[Int32])) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversedByStep01(): Bool = region rc {
        let expected = List#{22,17,12,7,2,-3};
        let v = MutList.new(rc);
        foreach (x <- rangeReversedByStep(closed(22, -3), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversedByStep02(): Bool = region rc {
        let expected = List#{22,17,12,7,2};
        let v = MutList.new(rc);
        foreach (x <- rangeReversedByStep(closedOpen(22, -3), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversedByStep03(): Bool = region rc {
        let expected = List#{20,15,10,5};
        let v = MutList.new(rc);
        foreach (x <- rangeReversedByStep(closedOpen(20, 0), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def rangeReversedByStep04(): Bool = region rc {
        let expected = List#{};
        let v = MutList.new(rc);
        foreach (x <- rangeReversedByStep(open(20, 20), 5)) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def toString01(): Bool =
        let expected = "[-1 , 1]";
        let r = closed(-1, 1);
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString02(): Bool =
        let expected = "[-1 , 1[";
        let r = closedOpen(-1, 1);
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString03(): Bool =
        let expected = "]-1 , 1]";
        let r = openClosed(-1, 1);
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString04(): Bool =
        let expected = "]-1 , 1[";
        let r = open(-1, 1);
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString05(): Bool =
        let expected = "[1 , +Infinite[";
        let r = atLeast(1);
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString06(): Bool =
        let expected = "]1 , +Infinite[";
        let r = greaterThan(1);
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString07(): Bool =
        let expected = "]-Infinite , 1]";
        let r = atMost(1) ;
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString08(): Bool =
        let expected = "]-Infinite , 1[";
        let r = lessThan(1) ;
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString09(): Bool =
        let expected = "]-Infinite , +Infinite[";
        let r: Range[Int32] = all();
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString10(): Bool =
        let expected = "Empty";
        let r: Range[Int32] = Range.Empty ;
        let actual = "${r}";
        Assert.eq(expected, actual)

}
