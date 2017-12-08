///
/// Represents the result of a comparison between two values.
///
enum Ordering {
    case Less,
    case Equal,
    case Greater
}

//
//    fn lt(&self, other: &Rhs) -> bool { ... }
//    fn le(&self, other: &Rhs) -> bool { ... }
//    fn gt(&self, other: &Rhs) -> bool { ... }
//    fn ge(&self, other: &Rhs) -> bool { ... }

///
/// Returns the smaller of `x` and `y`.
///
def min[a: Ord](x: a, y: a): a = ??? // TODO: Implement min.

///
/// Returns the larger of `x` and `y`.
///
def max[a: Ord](x: a, y: a): a = ??? // TODO: Implement max.
