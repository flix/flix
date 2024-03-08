///
/// We can define pure functions that use
/// internal mutability (impurity) with regions.
/// Regions encapsulate mutability to its declared scope.
///
def deduplicate(l: List[a]): List[a] with Order[a] =
    /// Declare a new region `rc`.
    region rc {

        /// Create a new `MutSet` at region `rc`.
        /// This will be used to keep track of
        /// unique elements in `l`.
        let s = MutSet.new(rc);

        /// The lambda used in the call to `filter`
        /// would be impure without a region.
        List.filter(x -> {
            if (MutSet.memberOf(x, s))
                false // `x` has already been seen.
            else {
                MutSet.add!(x, s);
                true
            }
        }, l)
    }

///
/// Create a list `l` with duplicates and
/// call `deduplicate` that returns a new list
/// with only unique elements.
///
def main(): Unit \ IO =
    let l = 1 :: 1 :: 2 :: 2 :: 3 :: 3 :: Nil;
    println(deduplicate(l))
