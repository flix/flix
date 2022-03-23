// We can define pure functions that use
// internal mutability (impurity) with regions.
// Regions encapsulate mutability to its declared scope.
def deduplicate(l: List[a]): List[a] with Order[a] =
    region r {                                   // Declare a new region `r`.
        let m = new MutSet(r);                   // Create a new `MutSet` using the region `r`.
        List.foreach(x -> MutSet.add!(x, m), l); // Add all elements in `l` to `m`.
        List.filter(x -> {                       // This function would be impure without a region.
            let p = MutSet.memberOf(x, m);       // Check if `x` is member of `m`.
            if (p) MutSet.remove!(x, m) else (); // If `x` is member of `m` then remove it
            p                                    // so no more elements equal to `x` are added to the list.
        }, l)
    }

// Create a list `l` with duplicates and
// call `deduplicate` that returns a new list
// with only unique elements.
def main(_args: Array[String]): Int32 & Impure =
    let l = 1 :: 1 :: 2 :: 2 :: 3 :: 3 :: Nil;
    println(deduplicate(l)); // Prints: 1 :: 2 :: 3 :: Nil
    0
