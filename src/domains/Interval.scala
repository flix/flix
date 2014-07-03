package domains

object Interval {
  def main(args: Array[String]): Unit = {

    // TODO: Intervals

    // Lattice Interval = Bot | Top | Range(Int, Int).

    // Interval.Leq(Bot, _).
    // Interval.Leq(Range(b1, e1), Range(b2, e2)) :- b2 <= b1, e2 >= e1.
    // Interval.Leq(_, Top).

    // Interval.Join(Bot, x, x).
    // Interval.Join(x, Bot, x).
    // Interval.Join(Range(b1, e1), Range(b2, e2), Range(b3, e3)) :- (max(e1, e2) - min(b1, b2)) <= 10.
    // Interval.Join(Range(b1, e1), Range(b2, e2), Top) :- (max(e1, e2) - min(b1, b2)) > 10.
    // Interval.Join(Top, _, Top).
    // Interval.Join(_, Top, Top).

    // Interval.Sum(Bot, _, Bot).
    // Interval.Sum(_, Bot, Bot).
    // Interval.Sum(Range(b1, e1), Range(b2, e2), Range(b1 + b2, e1 + e2)).
    // Interval.Sum(Top, _, Top).
    // Interval.Sum(_, Top, Top).
  }
}
