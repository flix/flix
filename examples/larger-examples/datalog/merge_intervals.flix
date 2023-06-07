/// https://leetcode.com/problems/merge-intervals/
/// Given a list of intervals where intervals[i] = [start_i, end_i], merge
/// all overlapping intervals, and return an array of the non-overlapping
/// intervals that cover all the intervals in the input.

def merge(intervals: List[(Int32, Int32)]): Vector[(Int32, Int32)] =
    let i = inject intervals into Interval;
    let lp = #{
        /// [a, ..., c, ..., b, ..., d]
        Interval(a, d) :-
            Interval(a, b), Interval(c, d),
            if a <= c and c <= b and b <= d.

        /// [a, ..., c, ..., d, ..., b]
        IsContained(c, d) :-
            Interval(a, b), Interval(c, d),
            if (a <= c and d < b) or (a < c and d <= b).

        Representative(a, b):- Interval(a, b), not IsContained(a, b).
    };
    query i, lp select (a, b) from Representative(a, b)

def main(): Unit \ IO =
    let intervals = (1, 3) :: (2, 6) :: (8, 10) :: (15, 18) :: Nil;
    merge(intervals) |> println // [(1, 6), (8, 10), (15, 18)]
