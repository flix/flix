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

//
// Acknowledgement: this is an (free) adaptation of `Data.Range` in the Haskell libraries maintained by Robert Massaioli
// and some ideas picked from https://github.com/google/guava/wiki/RangesExplained
//

pub enum BoundType with Eq, Order, ToString { case Inclusive, Exclusive } 

pub enum Bound[a]({ boundValue = a, boundType = BoundType })

instance Eq[Bound[a]] with Eq[a] {
    pub def eq(x: Bound[a], y: Bound[a]): Bool = 
        let Bound.Bound(xb) = x;
        let Bound.Bound(yb) = y;
        if ((xb.boundValue == yb.boundValue) and (xb.boundType == yb.boundType)) true
        else false
}

instance Order[Bound[a]] with Order[a] {
    pub def compare(x: Bound[a], y: Bound[a]): Comparison = 
        let Bound.Bound(xb) = x;
        let Bound.Bound(yb) = y;
        if (xb.boundValue < yb.boundValue) Comparison.LessThan
        else if (xb.boundValue > yb.boundValue) Comparison.GreaterThan
        else Comparison.EqualTo            
}

instance ToString[Bound[a]] with ToString[a] {
    pub def toString(b: Bound[a]) : String = 
        let Bound.Bound(bb)= b;
        "Value:${bb.boundValue} Type:${bb.boundType}"
}

instance Functor[Bound] {
    pub def map(f: a -> b \ ef, b: Bound[a]): Bound[b] \ ef = Data.Range.mapBound(f, b)
}

pub enum Range[a] with Eq, Order {
    case SpanRange(Bound[a],Bound[a])
    case LowerBoundRange(Bound[a])
    case UpperBoundRange(Bound[a])
    case InfiniteRange
    case Empty
}

instance Functor[Range] {
    pub def map(f: a -> b \ ef, r: Range[a]): Range[b] \ ef = Data.Range.map(f, r)
}

instance ToString[Range[a]] with ToString[a] {
    pub def toString(r: Range[a]) : String = Data.Range.toString(r)
}

pub enum IteratorFactory[a]({range = Range[a], producer =(Unit -> Option[a]) -> Option[a], isToBoundReached = a -> a -> Bool })

instance Iterable[IteratorFactory] {
    pub def iterator(rc: Region[r], it: IteratorFactory[a]): Iterator[a, r, r] \ r = Data.Range.iteratorBuilder(rc, it)
}

mod Data.Range {
    enum BoundEndPointType with Eq, ToString { case LowerBound, UpperBound }

    type alias Producer[a] = (Unit -> Option[a]) -> Option[a]
    type alias Predicate[a] = a -> a -> Bool

    pub def open(lowerBound:a, upperBound: a): Range[a] with Order[a] = 
        let lowerB = Bound.Bound({ boundValue = lowerBound , boundType = BoundType.Exclusive });
        let upperB = Bound.Bound({ boundValue = upperBound , boundType = BoundType.Exclusive });
        if (lowerB != upperB) Range.SpanRange(lowerB, upperB)
        else Range.Empty

    pub def closed(lowerBound:a, upperBound: a): Range[a] with Order[a] = 
        let lowerB = Bound.Bound({ boundValue = lowerBound , boundType = BoundType.Inclusive });
        let upperB = Bound.Bound({ boundValue = upperBound , boundType = BoundType.Inclusive });
        Range.SpanRange(lowerB, upperB)

    pub def closedOpen(lowerBound:a, upperBound: a): Range[a] with Order[a]  = 
        let lowerB = Bound.Bound({ boundValue = lowerBound , boundType = BoundType.Inclusive });
        let upperB = Bound.Bound({ boundValue = upperBound , boundType = BoundType.Exclusive });
        if (lowerB != upperB) Range.SpanRange(lowerB, upperB) 
        else Range.Empty

    pub def openClosed(lowerBound:a, upperBound: a): Range[a] with Order[a] = 
        let lowerB = Bound.Bound({ boundValue = lowerBound , boundType = BoundType.Exclusive });
        let upperB = Bound.Bound({ boundValue = upperBound , boundType = BoundType.Inclusive });
        if (lowerB != upperB) Range.SpanRange(lowerB, upperB)
        else Range.Empty

    pub def greaterThan(lowerBound:a): Range[a] = 
        let lowerB = Bound.Bound({ boundValue = lowerBound , boundType = BoundType.Exclusive });
        Range.LowerBoundRange(lowerB)

    pub def atLeast(lowerBound:a): Range[a] = 
        let lowerB = Bound.Bound({ boundValue = lowerBound , boundType = BoundType.Inclusive });
        Range.LowerBoundRange(lowerB)
    
    pub def lessThan(upperBound:a): Range[a] = 
        let upperB = Bound.Bound({ boundValue = upperBound , boundType = BoundType.Exclusive });
        Range.UpperBoundRange(upperB)

    pub def atMost(upperBound:a): Range[a] = 
        let upperB = Bound.Bound({ boundValue = upperBound , boundType = BoundType.Inclusive });
        Range.UpperBoundRange(upperB)
    
    pub def all(): Range[a] = 
        Range.InfiniteRange
  
    ///
    /// Returns a Bound with `f(boundValue))` and BoundType inchanged.
    ///
    pub def mapBound(f: a -> b \ ef, b: Bound[a]): Bound[b] \ ef = 
        let Bound.Bound(bb) = b;
        Bound.Bound({ boundValue = f(bb.boundValue) | bb })
           
    ///
    /// Returns a Range with `f(Bound))`.
    ///
    pub def map(f: a -> b \ ef, r: Range[a]): Range[b] \ ef = 
        match r {
            case Range.SpanRange(lowerB, upperB)    => Range.SpanRange(mapBound(f,lowerB),mapBound(f,upperB))
            case Range.LowerBoundRange(lowerB)      => Range.LowerBoundRange(mapBound(f,lowerB)) 
            case Range.UpperBoundRange(upperB)      => Range.UpperBoundRange(mapBound(f,upperB))
            case Range.InfiniteRange                => Range.InfiniteRange
            case Range.Empty                        => Range.Empty
        }   

    ///
    /// Renders the range `r` to a String.
    ///
    pub def toString(r: Range[a]): String with ToString[a] = match r {
        case Range.SpanRange(lowerB, upperB)    => "${innerToString(lowerB, BoundEndPointType.LowerBound)} , ${innerToString(upperB, BoundEndPointType.UpperBound)}"
        case Range.LowerBoundRange(lowerB)      => "${innerToString(lowerB, BoundEndPointType.LowerBound)} , +Infinite["
        case Range.UpperBoundRange(upperB)      => "]-Infinite , ${innerToString(upperB, BoundEndPointType.UpperBound)}"
        case Range.InfiniteRange                => "]-Infinite , +Infinite["
        case Range.Empty                        => "Empty"
    }

    // Helper function for `toString`.
    def innerToString(b: Bound[a], bept: BoundEndPointType): String with ToString[a] =
            let Bound.Bound(bb)= b;
            match (bb.boundValue, bb.boundType, bept) {
                case (_, BoundType.Inclusive, BoundEndPointType.LowerBound) => "[${bb.boundValue}"
                case (_, BoundType.Inclusive, BoundEndPointType.UpperBound) => "${bb.boundValue}]"
                case (_, BoundType.Exclusive, BoundEndPointType.LowerBound) => "]${bb.boundValue}"
                case (_, BoundType.Exclusive, BoundEndPointType.UpperBound) => "${bb.boundValue}["
            }

    ///
    /// Returns an iterator thanks an `IteratorFactory`
    ///
    pub def iteratorBuilder(rc: Region[r], it: IteratorFactory[a]): Iterator[a, r, r] \ r  = 
        let IteratorFactory.IteratorFactory(rg) = it;
        match rg.range {
            case Range.SpanRange(Bound.Bound(fromBound), Bound.Bound(toBound))   =>   
                                                            let curr = ref Some(fromBound.boundValue) @ rc;
                                                            let producer = deref (ref rg.producer @ rc);
                                                            
                                                            if (fromBound.boundType == BoundType.Exclusive)
                                                                let c0 = deref curr;
                                                                curr := producer(() -> c0)
                                                            else curr := deref curr;

                                                            let iterF = () -> match (deref curr) {
                                                                case Some(i) if rg.isToBoundReached(i, toBound.boundValue)  =>  let c = deref curr;
                                                                                                                                curr:= producer(() -> c); 
                                                                                                                                Some(i)
                                                                case _                                                      =>  None
                                                            };
                                                            Iterator.iterate(rc, iterF)
            case _                                      =>  Iterator.empty(rc)
        }

    ///
    /// Returns a `Bound` inverted. Means inclusive bound becomes exclusive and vice versa
    ///
    pub def invertBound(b: Bound[a]): Bound[a] = 
        let Bound.Bound(bb) = b;
        match bb {
            case ({boundType = BoundType.Inclusive | _ }) => Bound.Bound({ boundType = BoundType.Exclusive | bb })
            case ({boundType = BoundType.Exclusive | _ }) => Bound.Bound({ boundType = BoundType.Inclusive | bb })
        }

    ///
    /// Returns a `Range` reversed. Means lower bound becomes upper one and vice versa
    ///
    pub def reverseRange(r: Range[a]): Range[a] = 
        match r {
            case Range.SpanRange(lowerB, upperB)    => Range.SpanRange(upperB, lowerB)
            case Range.LowerBoundRange(lowerRange)  => Range.UpperBoundRange(lowerRange) 
            case Range.UpperBoundRange(upperRange)  => Range.LowerBoundRange(upperRange) 
            case _                                  => r
        }   

    ///
    /// Ability to generate a series of elements (i.e. various integer type)
    ///
    sealed pub trait Enumerable[a] {
        ///
        /// Returns a neutral (zero) element for type `a`.
        ///
        pub def zeroValue(): a

        ///
        /// Returns a neutral element (one) for type `a`.
        ///
        pub def oneValue(): a

        ///
        /// Returns an iterator of all integers between 0 (inclusive) to `toBound` (inclusive).
        ///
        pub def zeroTo(toBound: a): IteratorFactory[a] with Order[a], Add[a] = 
            Enumerable.rangeByStep(closed(Enumerable.zeroValue(), toBound), Enumerable.oneValue())

        ///
        /// Returns an iterator of all integers (descending order) between `fromBound` (inclusive) to `0` (inclusive).
        ///
        pub def toZero(fromBound: a): IteratorFactory[a] with Order[a], Sub[a] = 
            Enumerable.rangeReversedByStep(closed(fromBound, Enumerable.zeroValue()), Enumerable.oneValue())

        ///
        /// Returns an iterator of all integers (inclusive) included into the range `r` by step `step`.
        ///
        pub def rangeByStep(r: Range[a], step: a): IteratorFactory[a] with Order[a], Add[a] =
            /*let rg = match r {
                case Range.SpanRange(lowerB, upperB) if (lowerB <= upperB)  => r
                case _                                                      => Range.Empty
            };*/
            IteratorFactory.IteratorFactory({range = r, producer = nextByStep(step), isToBoundReached = predicateFromRange(r)})

        ///
        /// Returns an iterator of all integers (inclusive) included into the range `r`.
        ///
        pub def range(r: Range[a]): IteratorFactory[a] with Order[a], Add[a] = Enumerable.rangeByStep(r, Enumerable.oneValue())

        ///
        /// Returns an iterator of all integers (inclusive & descending order) included into the range `r` by step `step`.
        ///
        pub def rangeReversedByStep(r: Range[a], step: a): IteratorFactory[a] with Order[a], Sub[a] =
            let rg = match r {
                case Range.SpanRange(lowerB, upperB) if (lowerB >= upperB)  => r
                case _                                                      => Range.Empty
            };
            IteratorFactory.IteratorFactory({range = rg, producer = prevByStep(step), isToBoundReached = predicateFromRangeReversed(r)})

        ///
        /// Returns an iterator of all integers (inclusive & descending order) included into the range `r`.
        ///
        pub def rangeReversed(r: Range[a]): IteratorFactory[a] with Order[a], Sub[a] = Enumerable.rangeReversedByStep(r, Enumerable.oneValue())
    
    }

    instance Enumerable[Int8] {
        
        pub def zeroValue(): Int8 = 0i8
        pub def oneValue(): Int8 = 1i8
    
    }

    instance Enumerable[Int16] {
    
        pub def zeroValue(): Int16 = 0i16
        pub def oneValue(): Int16 = 1i16
       
    }

    instance Enumerable[Int32] {
     
        pub def zeroValue(): Int32 = 0i32
        pub def oneValue(): Int32 = 1i32
  
    }

    instance Enumerable[Int64] {
          
        pub def zeroValue(): Int64 = 0i64
        pub def oneValue(): Int64 = 1i64
  
    }

    ///
    /// Returns if the range `r` is empty or not.
    ///
    pub def isEmpty(r: Range[a]): Bool = match r {
            case Range.Empty    => true
            case _              => false
        }

    ///
    /// Returns if the range `r` is reversed or not.
    ///
    pub def isReversed(r: Range[a]): Bool with Order[a] = match r {
            case Range.SpanRange(lowerB, upperB)    => if (lowerB <= upperB) false else true
            case _                                  => false
        }



    // Returns a function which gives next value (in this module: only integer 8,16,32 & 64 types) by step `step`
    def nextByStep(step: a): Producer[a] with Add[a] =
        def next(e: Unit -> Option[a]): Option[a] = match e() {
            case Some(v)    => Some(v+step)
            case None       => None
        };
        next
    
    // Returns a function which gives previous value (in this module: only integer 8,16,32 & 64 types) by step `step`
    def prevByStep(step: a): Producer[a] with Sub[a] =
        def prev(e: Unit -> Option[a]): Option[a] = match e() {
            case Some(v)    => Some(v-step)
            case None       => None
        };
        prev

    def predicateFromRange(r: Range[a]): Predicate[a] with Order[a] = match r {
            case Range.SpanRange(_ , Bound.Bound(upperB)) if (upperB.boundType == BoundType.Inclusive)  => (curr, toBound) -> if (curr <= toBound) true else false
            case Range.SpanRange(_ , Bound.Bound(upperB)) if (upperB.boundType == BoundType.Exclusive)  => (curr, toBound) -> if (curr < toBound) true else false
            case _                                                                                      => (_, _) -> false
        }
    
    def predicateFromRangeReversed(r: Range[a]): Predicate[a] with Order[a] = match r {
            case Range.SpanRange(_, Bound.Bound(upperB)) if (upperB.boundType == BoundType.Inclusive)   => (curr, toBound) -> if (curr >= toBound) true else false
            case Range.SpanRange(_, Bound.Bound(upperB)) if (upperB.boundType == BoundType.Exclusive)   => (curr, toBound) -> if (curr > toBound) true else false
            case _                                                                                      => (_, _) -> false
        }

}
