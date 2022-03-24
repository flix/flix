// We can define pure functions that use
// internal mutability (impurity) with regions.
// Regions encapsulate mutability to its declared scope.
def deduplicate(l: List[a]): List[a] with Order[a] =
    // Declare a new region `r`.
    region r {

        // Create a new `MutSet` using the region `r`.
        let m = new MutSet(r);

        // Add all elements in `l` to `m`.
        List.foreach(x -> MutSet.add!(x, m), l);

        List.filter(x -> {
            // This function would be impure without a region.
            // Check if `x` is member of `m`.
            let p = MutSet.memberOf(x, m);

            // If `x` is member of `m` then remove it
            // so no more elements equal to `x` are added to the list.
            if (p) MutSet.remove!(x, m) else ();
            p
        }, l)
    }

// Create a list `l` with duplicates and
// call `deduplicate` that returns a new list
// with only unique elements.
def main(_args: Array[String]): Int32 & Impure =
    let l = 1 :: 1 :: 2 :: 2 :: 3 :: 3 :: Nil;
    println(deduplicate(l)); // Prints: 1 :: 2 :: 3 :: Nil
    0
