///
/// Represents the result of a comparison between two values.
///
pub enum Ordering {
    case Less,
    case Equal,
    case Greater
}

///
/// Returns `true` if `x` is strictly less than `y`.
///
pub def lt[a: Ord](_x: a, _y: a): Bool = ??? as Bool // TODO

///
/// Returns `true` if `x` is less than or equal to `y`.
///
pub def le[a: Ord](_x: a, _y: a): Bool = ??? as Bool // TODO

///
/// Returns `true` if `x` is strictly greater than `y`.
///
pub def gt[a: Ord](_x: a, _y: a): Bool = ??? as Bool // TODO

///
/// Returns `true` if `x` is greater than or equal to `y`.
///
pub def ge[a: Ord](_x: a, _y: a): Bool = ??? as Bool // TODO

///
/// Returns the smaller of `x` and `y`.
///
//def min[a: Ord](x: a, y: a): a = ??? // TODO: Implement min.

///
/// Returns the larger of `x` and `y`.
///
//def max[a: Ord](x: a, y: a): a = ???  // TODO: Implement max.
